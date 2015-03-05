(ns t6.from-scala.types
  (:refer-clojure :exclude [if-let for partial])
  (:require [clojure.core :as c]
            [cats.core :refer (mlet)]
            [cats.protocols :as protocols]
            [cats.monad.either :refer (left right from-either)]
            [potemkin :refer (def-map-type)]
            [t6.from-scala.internal :refer ($ $$) :as internal])
  (:import (clojure.lang Reflector Indexed)
           (scala.collection.immutable List List$ Seq)
           (scala.collection JavaConversions)
           (scala Product Option)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Working with Scala functions

(defn partial
  "Like `clojure.core/partial` but for Scala functions."
  {:added "0.1.0"}
  [f & args]
  (reduce (c/fn [acc x]
            (cond
              (instance? scala.Function0 acc)
              (throw
               (ex-info "Function with 0 arguments can't take any more arguments"
                        {:f acc :x x}))

              (instance? scala.Function1 acc)
              (internal/fn [] (.apply acc x))

              :else
              (.apply (.curried acc) x)))
          f
          args))

(defn wrap-fn
  {:added "0.1.0"}
  [f]
  (cond
    (instance? scala.Function1 f) f
    (ifn? f) (internal/fn [x] (f x))
    :else (throw (ex-info "Expected a Clojure or Scala function!" f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some convenience functions for Scala tuples and options

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
    (.getOrElse ^scala.Option this (internal/fn [] else)))

  Object
  (-option-get [this else]
    this)

  nil
  (-option-get [this else]
    else))

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

(defmacro if-let
  "Like `clojure.core/if-let` but with special handling of scala.Option.

 ($/if-let [x ($/option 1)] (inc x) :no) => 2
 ($/if-let [x ($/option nil)] (inc x) :no) => :no"
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
      (map (internal/fn [x] (tuple (.apply x 0) (.apply x 1))))
      (toMap -scalaPredef<:<-ev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Destructuring support for Scala collections, tuples and case classes

(defmulti view
  "Returns a map-like or indexed structure that wraps the given Scala
  object. The object can then participate in destructuring.

  ;; TODO: add tests for destructuring of case classes
  ($/view ($/option nil)) => nil
  ($/view ($/option :a)) => :a
  (let [[x y] ($/view ($/option ($/tuple 1 2)))] [x y]) => [1 2]
  (let [{:keys [_1 _3]} ($/view ($/tuple :a :b :c))]
    [_1 _3]) => [:a :c]
  (let [[x y] ($/view ($/tuple 1 2))]
    [x y]) => [1 2]"
  {:added "0.1.0"}
  class)

(def-map-type
  ProductView
  [^Product product]
  (get [this k default-value]
       (if (integer? k)
         (.nth this k default-value)
         (Reflector/invokeInstanceMember
          product
          (name (internal/encode-scala-symbol k)))))
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
       ($ m getOrElse k (internal/fn [] default-value)))
  (assoc [this k v] (throw (ex-info "assoc not supported!"
                                    {:this this, :key k, :value v})))
  (dissoc [this k] (throw (ex-info "dissoc not supported!"
                                   {:this this, :key k})))
  ;; TODO: Enable assoc, dissoc support?
  #_(assoc [this k v] (MapView. ($ m + (tuple k v))))
  #_(dissoc [this k] (MapView. ($ m - k)))
  (keys [this] (view ($ m keys))))

(defmethod view Option
  [this]
  (if-not (.isEmpty this)
    (view (.get this))))

(defmethod view scala.collection.Map
  [this]
  (MapView. this))

(defmethod view Product
  [this]
  (->ProductView this))

(defmethod view scala.collection.Iterable
  [this]
  ;; Do a `(map view ...)` on the result here? No, because we would need
  ;; to do the same for e.g. all members of scala.Products and for any
  ;; collection that participates in view, and this is probably a bad idea
  (iterator-seq
   (JavaConversions/asJavaIterator
    (.iterator this))))

(prefer-method view scala.collection.Iterable scala.Product)

(defmethod view :default
  [this]
  this)

(defmethod view nil
  [_]
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Monad, Functor and Applicative implementations for Scala collections

(extend-type scala.collection.Iterable
  protocols/Context
  (get-context [this] this)
  (get-value [this] this)

  protocols/Functor
  (fmap [this f v]
    ($ v map
       (wrap-fn f)
       ($$ this companion canBuildFrom)))

  protocols/Applicative
  (fapply [this self av]
    ($ self flatMap
       (internal/fn [f]
         ($ av map
            (wrap-fn f)
            ($$ this companion canBuildFrom)))
       ($$ this companion canBuildFrom)))

  (pure [this v]
    ($$ this companion (apply & v)))

  protocols/Monad
  (mreturn [this v]
    ($$ this companion (apply & v)))

  (mbind [this self f]
    ($ self flatMap (wrap-fn f)
       ($$ this companion canBuildFrom)))

  protocols/MonadZero
  (mzero [this]
    ($$ this companion empty))

  protocols/MonadPlus
  (mplus [this mv mv']
    ($ mv ++ mv' ($$ this companion canBuildFrom))))

(extend-type scala.Option
  protocols/Context
  (get-context [this] this)
  (get-value [this] this)

  protocols/Functor
  (fmap [this f v]
    ($ v map (wrap-fn f)))

  protocols/Applicative
  (fapply [this self av]
    ($ self flatMap (internal/fn [f] ($ av map (wrap-fn f)))))

  (pure [this v]
    (option v))

  protocols/Monad
  (mreturn [this v]
    (option v))

  (mbind [this self f]
    ($ self flatMap (wrap-fn f)))

  protocols/MonadZero
  (mzero [this]
    ($ scala.Option/empty))

  protocols/MonadPlus
  (mplus [this mv mv']
    (if-let [v (view mv)]
      (option v)
      mv')))

(defmacro for
  "List comprehension for Scala collections. Syntax follows
  `clojure.core/for`, however `for` is not lazy. Returns a collection
  with the same type as the type of the first collection you iterate
  over.

  This is syntactic sugar for `cats.core/mlet`.

  ($/for [x ($ Set & 1 2 3) :when (even? x)] x) => ($ Set & 2)
  ($/for [x ($ List & 1), y ($ Set & 2)] [x y]) => ($ List & [1 2])

  ($/for [x ($/option 1)] x) => ($/option 1)
  ($/for [x ($/option 1) y ($/option 2)] (+ x y)) => ($/option 3)
  ($/for [x ($/option nil) y ($/option 2)] (+ x y)) => ($/option nil)"
  {:added "0.1.0"}
  [bindings expr]
  `(cats.core/mlet
    ~bindings
    (cats.core/return ~expr)))
