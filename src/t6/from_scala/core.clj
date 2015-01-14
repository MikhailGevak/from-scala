(ns t6.from-scala.core
  (:refer-clojure :exclude [partial for if-let])
  (:require [clojure.core :as c]
            [clojure.string :as str]
            [cats.core :refer (mlet)]
            [cats.protocols :as protocols]
            [cats.monad.either :refer (left right from-either)]
            [potemkin :refer (def-map-type)])
  (:import (scala.reflect NameTransformer$)
           (clojure.lang Reflector Indexed IFn)
           (scala.collection.immutable List List$ Seq)
           (scala.collection JavaConversions)
           (scala Product Option)))

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
  "safe-resolve"
  {:added "0.1.0"}
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
      (fn [i x]
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

(defn static-call
  [{class-name :class, method-name :method :as m}]
  (merge m
         {:emit :dot-special-form}
         (c/if-let [class (safe-resolve class-name)]
           (if (class? class)
             (let [fqn-class (symbol (.getName ^Class class))
                   method (encode-scala-symbol method-name)
                   companion (-> (str fqn-class "$") safe-resolve)]
               (if (and companion (class? companion))
                 {:expr (symbol (.getName ^Class companion)
                                "MODULE$")
                  :method method}
                 {:expr  fqn-class
                  :method method}))
             (throw (ex-info "Not a class" class-name)))
           (throw (ex-info "Cannot find class with that name in this namespace!"
                           {:namespace *ns*
                            :name class-name})))))

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
     ($/function [acc x] (+ acc x))) => 6"
  {:added "0.1.0"}
  [expr & args]
  (when debug
    (print "orig form: ")
    (prn &form)
    (prn (meta &form))
    (prn &env))
  (let [;; This will try each expression in order and return the first `left` value.
        ;; `failure` throws an exception otherwise, leave it in the last position
         m (from-either
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
                                              (t6.from-scala.core/$ a)
                                              (t6.from-scala.core/$ b)
                                              (t6.from-scala.core/$ c))"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create Scala functions

(defmacro function
  "Returns a new Scala function.

  ($/function [] :foo) => (instance-of scala.Function0)
  ($/function [x] (inc x)) => (instance-of scala.Function1)
  ($/function [a b c d e f g h i j k l m n o p q r s t]) => (instance-of scala.Function20)
  ($/function [a b c d e f g h i j k l m n o p q r s t u]) => (instance-of scala.Function21)
  ($/function [a b c d e f g h i j k l m n o p q r s t u v]) => (instance-of scala.Function22)
  (eval '($/function [a b c d e f g h i j k l m n o p q r s t u v w])) => (throws ClassNotFoundException \"scala.Function23\")
  (.apply ($/function [x] (inc x)) 1) => 2"
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

(defn partial
  "Like `clojure.core/partial` but for Scala functions."
  {:added "0.1.0"}
  [f & args]
  (reduce (fn [acc x]
            (cond
             (instance? scala.Function0 acc)
             (throw
              (ex-info "Function with 0 arguments can't take any more arguments"
                       {:f acc :x x}))

             (instance? scala.Function1 acc)
             (function [] (.apply acc x))

             :else
             (.apply (.curried acc) x)))
          f
          args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some convenience functions for Scala tuples and options

(defn tuple
  "Returns a Scala tuple. Uses the Scala tuple class that matches the
  number of arguments.

  ($/tuple 1 2) => (instance-of scala.Tuple2)
  (apply $/tuple (range 20)) => (instance-of scala.Tuple20)
  (apply $/tuple (range 21)) => (instance-of scala.Tuple21)
  (apply $/tuple (range 22)) => (instance-of scala.Tuple22)
  (apply $/tuple (range 23)) => (throws ExceptionInfo)"
  {:added "0.1.0"}
  ([a]
     (scala.Tuple1. a))
  ([a b]
     (scala.Tuple2. a b))
  ([a b c]
     (scala.Tuple3. a b c))
  ([a b c d]
     (scala.Tuple4. a b c d))
  ([a b c d e]
     (scala.Tuple5. a b c d e))
  ([a b c d e f]
     (scala.Tuple6. a b c d e f))
  ([a b c d e f g]
     (scala.Tuple7. a b c d e f g))
  ([a b c d e f g h]
     (scala.Tuple8. a b c d e f g h))
  ([a b c d e f g h i]
     (scala.Tuple9. a b c d e f g h i))
  ([a b c d e f g h i j]
     (scala.Tuple10. a b c d e f g h i j))
  ([a b c d e f g h i j k]
     (scala.Tuple11. a b c d e f g h i j k))
  ([a b c d e f g h i j k l]
     (scala.Tuple12. a b c d e f g h i j k l))
  ([a b c d e f g h i j k l m]
     (scala.Tuple13. a b c d e f g h i j k l m))
  ([a b c d e f g h i j k l m n]
     (scala.Tuple14. a b c d e f g h i j k l m n))
  ([a b c d e f g h i j k l m n o]
     (scala.Tuple15. a b c d e f g h i j k l m n o))
  ([a b c d e f g h i j k l m n o p]
     (scala.Tuple16. a b c d e f g h i j k l m n o p))
  ([a b c d e f g h i j k l m n o p q]
     (scala.Tuple17. a b c d e f g h i j k l m n o p q))
  ([a b c d e f g h i j k l m n o p q r]
     (scala.Tuple18. a b c d e f g h i j k l m n o p q r))
  ([a b c d e f g h i j k l m n o p q r s]
     (scala.Tuple19. a b c d e f g h i j k l m n o p q r s))
  ([a b c d e f g h i j k l m n o p q r s t]
     (scala.Tuple20. a b c d e f g h i j k l m n o p q r s t))
  ([a b c d e f g h i j k l m n o p q r s t & [u v :as args]]
     (case (count args)
       1 (scala.Tuple21. a b c d e f g h i j k l m n o p q r s t u)
       2 (scala.Tuple22. a b c d e f g h i j k l m n o p q r s t u v)
       (throw (ex-info "Can only create Scala tuples with up to 22 elements"
                       {:count (+ 20 (count args))})))))

(defn option
  "Returns a Scala option.

  ($/option nil) => (instance-of scala.None$)
  ($/option :a) => (instance-of scala.Some)"
  {:added "0.1.0"}
  [o]
  ($ Option o))

(defprotocol OptionGetOrElse
  (-option-get [this else]))

(extend-protocol OptionGetOrElse
  scala.Option
  (-option-get [this else]
    (.getOrElse ^scala.Option this (function [] else)))

  Object
  (-option-get [this else]
    this)

  nil
  (-option-get [this else]
    else))

(defmacro if-let
  "Like `clojure.core/if-let` but with special handling of scala.Option."
  {:added "0.2.0"}
  [binding then else]
  (let [[x expr] binding]
    `(let [v# ~expr]
       (c/if-let [~x (-option-get v# nil)]
         ~then
         ~else))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constructing Scala data structures

(defn immutable-list
  {:added "0.2.0"}
  [& xs]
  (c/if-let [xs (seq xs)]
    ($$ scala.collection.JavaConverters/asScalaBufferConverter xs
        asScala toList)
    ($ scala.collection.immutable.List/empty)))

(defn immutable-set
  {:added "0.2.0"}
  [& xs]
  (c/if-let [xs (seq xs)]
    ($$ scala.collection.JavaConverters/asScalaBufferConverter xs
        asScala toSet)
    ($ scala.collection.immutable.Set/empty)))

(defn immutable-vector
  {:added "0.2.0"}
  [& xs]
  (c/if-let [xs (seq xs)]
    ($$ scala.collection.JavaConverters/asScalaBufferConverter xs
        asScala toVector)
    ($ scala.collection.immutable.Vector/empty)))

(def -scalaPredef<:<-ev
  (proxy [scala.Predef$$less$colon$less] []
    (apply [x] x)))

(defn immutable-map
  {:added "0.2.0"}
  [& xs]
  ($$ scala.collection.JavaConverters/asScalaBufferConverter xs
      asScala
      (grouped 2)
      (map (function [x] (tuple (.apply x 0) (.apply x 1))))
      (toMap -scalaPredef<:<-ev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Destructuring support for Scala collections, tuples and case classes

(defprotocol IView
  (view [object]
    "Returns a map-like or indexed structure that wraps the given Scala
    object. The object can then participate in destructuring."))

(def-map-type
  ProductView
  [^Product product]
  (get [this k default-value]
       (if (integer? k)
         (.nth this k default-value)
         (Reflector/invokeInstanceMember
          product
          (name (encode-scala-symbol k)))))
  (assoc [this k v] (throw (ex-info "assoc not supported!"
                                    {:this this, :key k, :value v})))
  (dissoc [this k] (throw (ex-info "dissoc not supported!"
                                   {:this this, :key k})))
  (keys [_] (range 0 (.productArity product)))

  Object
  (equals [this that]
          (and (instance? ProductView that)
               (.equals product (.product that))))
  (hashCode [_] (.hashCode product))

  Indexed
  (nth [_ ^int i]
       (.productElement product i))
  (nth [_ ^int i not-found]
       (try
         (.productElement product (int i))
         (catch IndexOutOfBoundsException e
           not-found)))
  (count [_] (.productArity product)))

(def-map-type
  MapView
  [^scala.collection.Map m]
  (get [this k default-value]
       ($ m getOrElse k (function [] default-value)))
  (assoc [this k v] (throw (ex-info "assoc not supported!"
                                    {:this this, :key k, :value v})))
  (dissoc [this k] (throw (ex-info "dissoc not supported!"
                                   {:this this, :key k})))
  ;; TODO: Enable assoc, dissoc support?
  #_(assoc [this k v] (MapView. ($ m + (tuple k v))))
  #_(dissoc [this k] (MapView. ($ m - k)))
  (keys [this] (view ($ m keys))))

(extend-protocol IView
  scala.Product
  (view [this] (->ProductView this))

  scala.Option
  (view [this]
    (view
     (if-not (.isEmpty this)
       (view (.get this)))))

  scala.collection.Map
  (view [this]
    (MapView. this))

  scala.collection.Iterable
  (view [this]
    ;; Do a `(map view ...)` on the result here? No, because we would need
    ;; to do the same for e.g. all members of scala.Products and for any
    ;; collection that participates in IView, and this is probably a bad idea
    (iterator-seq
     (JavaConversions/asJavaIterator
      (.iterator this))))

  Object
  (view [this] this)

  nil
  (view [this] nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Monad, Functor and Applicative implementations for Scala collections

(defn wrap-function
  {:added "0.1.0"}
  [f]
  (cond
    (instance? scala.Function1 f) f
    (ifn? f) (function [x] (f x))
    :else (throw (ex-info "Expected a Clojure or Scala function!" f))))

(extend-type scala.collection.Iterable
  protocols/Context
  (get-context [this] this)
  (get-value [this] this)

  protocols/Functor
  (fmap [this f v]
    ($ v map
       (wrap-function f)
       ($$ this companion canBuildFrom)))

  protocols/Applicative
  (fapply [this self av]
    ($ self flatMap
       (function [f]
                 ($ av map
                    (wrap-function f)
                    ($$ this companion canBuildFrom)))
       ($$ this companion canBuildFrom)))

  (pure [this v]
    ($$ this companion (apply & v)))

  protocols/Monad
  (mreturn [this v]
    ($$ this companion (apply & v)))

  (mbind [this self f]
    ($ self flatMap (wrap-function f)
       ($$ this companion canBuildFrom)))

  protocols/MonadZero
  (mzero [this]
    ($$ this companion empty))

  protocols/MonadPlus
  (mplus [this mv mv']
    ($ mv ++ mv' ($$ this companion canBuildFrom))))

(defmacro for
  "List comprehension for Scala collections. Syntax follows
  `clojure.core/for`, however `for` is not lazy. Returns a collection
  with the same type as the type of the first collection you iterate
  over.

  This is syntactic sugar for `cats.core/mlet`.

  ($/for [x ($ Set & 1 2 3) :when (even? x)] x) => ($ Set & 2)
  ($/for [x ($ List & 1), y ($ Set & 2)] [x y]) => ($ List & [1 2])"
  {:added "0.1.0"}
  [bindings expr]
  `(cats.core/mlet
     ~bindings
     (cats.core/return ~expr)))
