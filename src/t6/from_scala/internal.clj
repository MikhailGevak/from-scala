(ns t6.from-scala.internal
  (:refer-clojure :exclude [fn])
  (:require [clojure.core :as c]
            [clojure.string :as str]
            [cats.core :refer (mlet)]
            [cats.protocols :as protocols]
            [cats.monad.either :refer (left right)])
  (:import (scala.reflect NameTransformer$)
           (clojure.lang Reflector)))

(def debug
  "Set to `true` to enable debug messages when invoking $."
  false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Decode/encode Scala symbols

(defn decode-scala-symbol
  "Transforms a Java identifier to a Scala identifier. Accepts
  strings, symbols or keywords. Returns a string.

 ($/decode-scala-symbol \"foo.$plus$plus.bar.$colon$colon\") => \"foo.++.bar.::\"
 ($/decode-scala-symbol \"$plus$plus\") => \"++\"
 ($/decode-scala-symbol '$lessinit$greater) => \"<init>\""
  {:added "0.1.0"}
  [s]
  ;; split will also remove any potential duplicate or suffix dots in s
  (let [packages (str/split (name s) #"\.")]
    (->> packages
         (map #(.decode NameTransformer$/MODULE$ %1))
         (str/join "."))))

(defn encode-scala-symbol
  "Transforms a Scala identifier (e.g. method or class names) to a
  Java identifier. Accepts strings, symbols or keywords.  Returns a
  Clojure symbol.

 ($/encode-scala-symbol \"foo.++.bar.::\") => 'foo.$plus$plus.bar.$colon$colon
 ($/encode-scala-symbol \"++\") => '$plus$plus
 ($/encode-scala-symbol '<init>) => '$lessinit$greater
 ;; trailing dots are removed
 ($/encode-scala-symbol \"::.\") => '$colon$colon"
  {:added "0.1.0"}
  [s]
  (let [packages (str/split (name s) #"\." )]
    (->> packages
         (map #(.encode NameTransformer$/MODULE$ %1))
         (str/join ".")
         symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resolve and invoke Scala classes and methods

(defn safe-resolve
  [sym]
  (try
    (resolve (encode-scala-symbol sym))
    (catch Exception e
      nil)))

(defn var-args
  "Returns a scala immutable seq with the given arguments. This is
  used by `$` when calling Scala methods that expect variable
  arguments.

  ($/var-args 1 2 3) => (instance-of scala.collection.Seq)
  (.apply ($/var-args 1 2 3) 0) => 1
  (.apply ($/var-args 1 2 3) 1) => 2
  (.apply ($/var-args 1 2 3) 2) => 3
  (.length ($/var-args)) => 0"
  {:added "0.1.0"}
  ^scala.collection.immutable.Seq
  [& args]
  ;; Used by the $ macro so we need to use normal java interop here
  (if (seq args)
    (.. scala.collection.JavaConverters$/MODULE$
        (asScalaBufferConverter args)
        asScala
        ;; this will return a Scala Vector which implements immutable.Seq
        toIndexedSeq)
    (.empty scala.collection.immutable.Vector$/MODULE$)))

(defmulti emit-form :emit)

(defmethod emit-form :dot-special-form-static
  [{:keys [expr method args symbol]}]
  `(Reflector/invokeStaticMethod ~expr ~(str method) (into-array [~@args])))

(defmethod emit-form :dot-special-form
  [{:keys [expr method args symbol]}]
  `(. ~expr ~method ~@args))

(defmethod emit-form :passthrough
  [{:keys [expr]}]
  expr)

(defmethod emit-form :new-special-form
  [{:keys [expr args]}]
  `(Reflector/invokeConstructor ~expr (object-array (list ~@args))))

(defn expand-var-args
  [{:keys [args] :as m}]
  (assoc m
    :args
    ;; Examples: (args, partition, result)
    (let [[x y z :as parts] (partition-by #(= '& %1) args)
          n (count parts)]
      (cond
        ;; case 1: [] => [] => []
        (zero? n)
        ()

        ;; case 2: [&] => [(&)] => [(immutable-list)]
        (and (= n 1) (= x '(&)))
        `((var-args))

        ;; case 3: [:a :b :c] => [(:a :b :c)] => [:a :b :c]
        (= n 1)
        x

        ;; case 4: [& :a] => [(&) (:a)] => [(immutable-list :a)]
        (and (= n 2) (= x '(&)))
        (do
          (assert (every? #(not= '_ %1) y)
                  "default arguments in varargs make no sense")
          `((var-args ~@y)))

        ;; case 5: [:a & :b] => [(:a) (&) (:b)] => [:a (immutable-list :b)]
        :else
        (do
          (assert (every? #(not= '_ %1) z)
                  "default arguments in varargs make no sense")
          `(~@x (var-args ~@z)))))))

(defn expand-default-args
  [{:keys [expr args method] :as m}]
  ;; search for occurrences of _ representing a default argument
  ;; For the default arguments in a method `foo` Scala generates a method named
  ;; `foo$default$X` where X is the position (1-based) of the argument
  ;; All _ should be replaced with the appropriate call to the default argument
  ;; methods. E.g. `($ x foo _ :a _)` expands to
  ;; `($ x foo (.foo$default$1 x) :a (.foo$default$3 x))`
  (assoc m
    :args
    (map-indexed
      (c/fn [i x]
        (if (= x '_)
          `(~(symbol (str "." method "$default$" (inc i))) ~expr)
          x))
      args)))

(defn constructor-call-emit
  [expr m]
  (c/if-let [c (safe-resolve (encode-scala-symbol expr))]
    (left (assoc m :emit :new-special-form
                 :expr (symbol (.getName c))))
    (right)))

(defn symbol-constructor-call
  [{:keys [expr meta] :as m}]
  (if (symbol? expr)
    (if (.endsWith (name expr) ".")
      (constructor-call-emit expr m)
      (if (:new meta)
        (constructor-call-emit expr m)
        (right)))
    (right)))

(defn string-constructor-call
  [{:keys [expr] :as m}]
  (if (and (string? expr)
           (.endsWith expr "."))
    (constructor-call-emit expr m)
    (right)))

(defn has-field?
  [class field]
  (if class
    (try
      (.getField class (str field))
      true
      (catch NoSuchFieldException e
        false))
    false))

(defn get-companion
  [class]
  (let [fqn-class (safe-resolve (.getName ^Class class))]
    (if (has-field? fqn-class "MODULE$")
      fqn-class
      (let [companion (-> (str (.getName ^Class fqn-class) "$") safe-resolve)]
        (if (has-field? companion "MODULE$")
          companion
          nil)))))

(defn static-call
  [{class-name :class, method-name :method :as m}]
  (merge m
         {:emit :dot-special-form}
         (c/if-let [class (safe-resolve class-name)]
           (if (class? class)
             (let [fqn-class (symbol (.getName ^Class class))
                   method (encode-scala-symbol method-name)
                   companion (get-companion class)]
               (if companion
                 {:expr (symbol (.getName ^Class companion) "MODULE$")
                  :method method}
                 {:emit :dot-special-form-static
                  :expr  fqn-class
                  :method method}))
             (throw (ex-info "Not a class" {:name class-name})))
           ;; Maybe the class we look for is compiled as a Scala object only.
           ;; So append $ and look for a Scala object
           (c/if-let [class (safe-resolve (str class-name "$"))]
             {:expr (symbol (.getName ^Class class) "MODULE$")
              :method (encode-scala-symbol method-name)}
             (throw (ex-info "Cannot find a class with that name in this namespace or on the classpath!"
                             {:namespace *ns*
                              :name class-name}))))))

(defn static-call-vector
  [{:keys [expr] :as m}]
  (if (and (vector? expr)
           (= 2 (count expr)))
    (let [[class-name method-name] expr]
      ;; left -> success, do not look further (short circuits monad expression)
      (left (static-call (assoc m
                           :class class-name
                           :method method-name))))
    ;; right -> fail
    (right)))

(defn static-call-symbol
  [{:keys [expr] :as m}]
  (if (and (symbol? expr)
           (namespace expr))
    (left (static-call (assoc m
                         :class (namespace expr)
                         :method (name expr))))
    (right)))

(defn companion-apply
  [{:keys [expr] :as m}]
  ;; if the symbol resolves to a class, treat it as a call to apply of
  ;; the class' companion object
  (let [class (c/if-let [class (safe-resolve expr)]
                ;; if expr resolved to a var, try derefing it
                ;; this way we can define var aliases to scala classes
                ;; this only makes sense here, which is why this is not
                ;; in safe-resolve
                (if (var? class) @class class))]
    (if (class? class)
      (left (static-call (assoc m
                           :class (.getName ^Class class)
                           :method 'apply)))
      (right))))

(defn instance-call
  [{:keys [args] :as m}]
  (assoc m
    :emit :dot-special-form
    :method (encode-scala-symbol (first args))
    :args (next args)))

(defn regular-instance-call
  [{:keys [args] :as m}]
  ;; Assume the expression is refering to an instance
  (if (> (count args) 0)
    (left (instance-call m))
    (right)))

(defn passthrough
  [{:keys [args] :as m}]
  (if (= (count args) 0)
    ;; pass-through expression
    (left (assoc m :emit :passthrough))
    (right)))

(defn failure
  [m]
  (throw (ex-info "Failed with expression" m)))

(defmacro $
  "Scala interop macro. Method and class symbols can be strings if you
  need to use Scala classes, objects or method names that cannot be
  represented as a Clojure symbol.

  ;; Calls companion object scala.Option$/MODULE$
  (.get ($ scala.Option :a)) => :a
  ;; Calls List's companion object apply method with variable arguments
  ($ List & 1 2 3) => (instance-of List)
  (.length ($ List & 1 2 3)) => 3
  ;; call method on companion object
  ($ List/empty) => (instance-of List)
  ;; alt. syntax for special method/class names (that are invalid Clojure symbols)
  ;; TODO: better examples
  ($ [List empty]) => (instance-of List)
  ($ [List \"empty\"]) => (instance-of List)
  ($ [\"List\" \"empty\"]) => (instance-of List)
  ;; use $colon$colon for constructing a list
  (.length ($ ($ List/empty) \"::\" 1)) => 1
  ;; construct a Scala class
  (.get ($ scala.Some. :a)) => :a
  ;; use Scala's collection API
  ($ ($ List & 1 2 3)
     reduce
     ($/fn [acc x] (+ acc x))) => 6"
  {:added "0.1.0"}
  [expr & args]
  (when debug
    (print "orig form: ")
    (prn &form)
    (prn (meta &form))
    (prn &env))
  (let [;; This will try each expression in order and return the first `left` value.
        ;; `failure` throws an exception otherwise, leave it in the last position
         m (deref
             (mlet [m (right {:expr expr
                              :meta (meta expr)
                              :args args})
                    _ (symbol-constructor-call m)
                    _ (string-constructor-call m)
                    _ (static-call-vector m)
                    _ (static-call-symbol m)
                    _ (companion-apply m)
                    _ (regular-instance-call m)
                    _ (passthrough m)
                    _ (failure m)]))]
    (when debug
      (print "emit-form: ")
      (prn m))
    (let [;; a symbol representing the expression
           sym (gensym "$__")
          ;; Make sure the expression we emit is a simple symbol, so
          ;; that we can refer to it during args expansion if necessary
           v `(let [~sym ~(:expr m)]
                ~(-> (assoc m :expr sym)
                     expand-var-args
                     expand-default-args
                     emit-form))]
      (when debug
        (print "emitted: ")
        (prn v))
      v)))

(defmacro $$
  "A threading version of `$`.

  ($$ x) =expands-to=> (clojure.core/-> x)
  ($$ x a b c) =expands-to=> (clojure.core/-> x
                                              (t6.from-scala.internal/$ a)
                                              (t6.from-scala.internal/$ b)
                                              (t6.from-scala.internal/$ c))"
  {:added "0.1.0"}
  [x & forms]
  `(-> ~x
       ~@(clojure.core/for [f forms]
           (cond
             (list? f)
             `($ ~@f)

             (or (symbol? f)
                 (string? f))
             `($ ~f)

             :else
             (throw (ex-info "Malformed expression" f))))))

(defmacro fn
  "Returns a new Scala function.

  ($/fn [] :foo) => (instance-of scala.Function0)
  ($/fn [x] (inc x)) => (instance-of scala.Function1)
  ($/fn [a b c d e f g h i j k l m n o p q r s t]) => (instance-of scala.Function20)
  ($/fn [a b c d e f g h i j k l m n o p q r s t u]) => (instance-of scala.Function21)
  ($/fn [a b c d e f g h i j k l m n o p q r s t u v]) => (instance-of scala.Function22)
  (eval '($/fn [a b c d e f g h i j k l m n o p q r s t u v w])) => (throws ClassNotFoundException \"scala.Function23\")
  (.apply ($/fn [x] (inc x)) 1) => 2"
  {:added "0.1.0"}
  [params & body]
  (let [arity (count params)
        class (symbol (str "scala.Function" arity))
        defimpl (symbol (str class "$class"))]
    `(reify ~class
       (~'apply [this# ~@params]
        ~@body)
       (~'toString [this#]
        ($ [~defimpl ~'toString] this#))
       ~@(cond
           (= arity 1)
           `((~'andThen [this# g#]
              ;; Call the default implementation for andThen
              ($ [~defimpl ~'andThen] this# g#))
             (~'compose [this# g#]
              ($ [~defimpl ~'compose] this# g#)))
           (> arity 1)
           `((~'curried [this#]
              ($ [~defimpl ~'curried] this#))
             (~'tupled [this#]
                       ($ [~defimpl ~'tupled] this#)))))))
