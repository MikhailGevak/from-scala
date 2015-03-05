(ns t6.from-scala.core-test
  (:refer-clojure :exclude [for partial])
  (:use midje.sweet)
  (:require [t6.from-scala.core :refer ($ $$) :as $]
            [t6.from-scala.internal :refer (encode-scala-symbol decode-scala-symbol)])
  (:import (scala.collection.immutable List Set)
           (clojure.lang ExceptionInfo)))

(defchecker instance-of [expected-class]
  (checker [object] (instance? expected-class object)))

[[:chapter {:title "Scala interop"}]]

^{:refer t6.from-scala.internal/$ :added "0.1.0"}
(fact
  "Scala interop macro. Method and class symbols can be strings if you
  need to use Scala classes, objects or method names that cannot be
  represented as a Clojure symbol.\n"
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
  ($ [List "empty"]) => (instance-of List)
  ($ ["List" "empty"]) => (instance-of List)
  ;; use $colon$colon for constructing a list
  (.length ($ ($ List/empty) "::" 1)) => 1
  ;; construct a Scala class
  (.get ($ scala.Some. :a)) => :a
  ;; use Scala's collection API
  ($ ($ List & 1 2 3)
     reduce
     ($/fn [acc x] (+ acc x))) => 6)

^{:refer t6.from-scala.internal/$$ :added "0.1.0"}
(fact
  "A threading version of `$`.\n"
  ($$ x) =expands-to=> (clojure.core/-> x)
  ($$ x a b c) =expands-to=> (clojure.core/-> x
                                              (t6.from-scala.internal/$ a)
                                              (t6.from-scala.internal/$ b)
                                              (t6.from-scala.internal/$ c)))

^{:refer t6.from-scala.internal/var-args :added "0.1.0"}
(fact
  "Returns a scala immutable seq with the given arguments. This is
  used by `$` when calling Scala methods that expect variable
  arguments.\n"
  ($/var-args 1 2 3) => (instance-of scala.collection.Seq)
  (.apply ($/var-args 1 2 3) 0) => 1
  (.apply ($/var-args 1 2 3) 1) => 2
  (.apply ($/var-args 1 2 3) 2) => 3
  (.length ($/var-args)) => 0)

^{:refer t6.from-scala.internal/fn :added "0.1.0"}
(fact
  "Returns a new Scala function.\n"
  ($/fn [] :foo) => (instance-of scala.Function0)
  ($/fn [x] (inc x)) => (instance-of scala.Function1)
  ($/fn [a b c d e f g h i j k l m n o p q r s t]) => (instance-of scala.Function20)
  ($/fn [a b c d e f g h i j k l m n o p q r s t u]) => (instance-of scala.Function21)
  ($/fn [a b c d e f g h i j k l m n o p q r s t u v]) => (instance-of scala.Function22)
  (eval '($/fn [a b c d e f g h i j k l m n o p q r s t u v w])) => (throws ClassNotFoundException "scala.Function23")
  (.apply ($/fn [x] (inc x)) 1) => 2)

^{:refer t6.from-scala.types/partial :added "0.1.0"}
(fact
  "Like `clojure.core/partial` but for Scala functions.\n")

[[:chapter {:title "Working with Scala tuples, options and collections"}]]

^{:refer t6.from-scala.types/tuple :added "0.1.0"}
(fact
  "Returns a Scala tuple. Uses the Scala tuple class that matches the
  number of arguments.\n"
  ($/tuple 1 2) => (instance-of scala.Tuple2)
  (apply $/tuple (range 20)) => (instance-of scala.Tuple20)
  (apply $/tuple (range 21)) => (instance-of scala.Tuple21)
  (apply $/tuple (range 22)) => (instance-of scala.Tuple22)
  (apply $/tuple (range 23)) => (throws ExceptionInfo))

^{:refer t6.from-scala.types/option :added "0.1.0"}
(fact
  "Returns a Scala option.\n"
  ($/option nil) => (instance-of scala.None$)
  ($/option :a) => (instance-of scala.Some))

^{:refer t6.from-scala.types/for :added "0.1.0"}
(fact
  "List comprehension for Scala collections. Syntax follows
  `clojure.core/for`, however `for` is not lazy. Returns a collection
  with the same type as the type of the first collection you iterate
  over.

  This is syntactic sugar for `cats.core/mlet`.\n"
  ($/for [x ($ Set & 1 2 3) :when (even? x)] x) => ($ Set & 2)
  ($/for [x ($ List & 1), y ($ Set & 2)] [x y]) => ($ List & [1 2])

  ($/for [x ($/option 1)] x) => ($/option 1)
  ($/for [x ($/option 1) y ($/option 2)] (+ x y)) => ($/option 3)
  ($/for [x ($/option nil) y ($/option 2)] (+ x y)) => ($/option nil))

[[:chapter {:title "Destructuring support for Scala collections, tuples and case classes."}]]

^{:refer t6.from-scala.types/view :added "0.1.0"}
(fact
  "Returns a map-like or indexed structure that wraps the given Scala
  object. The object can then participate in destructuring.\n"
  ;; TODO: add tests for destructuring of case classes
  ($/view ($/option nil)) => nil
  ($/view ($/option :a)) => :a
  (let [[x y] ($/view ($/option ($/tuple 1 2)))] [x y]) => [1 2]
  (let [{:keys [_1 _3]} ($/view ($/tuple :a :b :c))]
    [_1 _3]) => [:a :c]
  (let [[x y] ($/view ($/tuple 1 2))]
    [x y]) => [1 2])

[[:chapter {:title "Utility functions"}]]

^{:refer t6.from-scala.internal/decode-scala-symbol :added "0.1.0"}
(fact
  "Transforms a Java identifier to a Scala identifier. Accepts
  strings, symbols or keywords. Returns a string.\n"
  (decode-scala-symbol "foo.$plus$plus.bar.$colon$colon") => "foo.++.bar.::"
  (decode-scala-symbol "$plus$plus") => "++"
  (decode-scala-symbol '$lessinit$greater) => "<init>")

^{:refer t6.from-scala.internal/encode-scala-symbol :added "0.1.0"}
(fact
  "Transforms a Scala identifier (e.g. method or class names) to a
  Java identifier. Accepts strings, symbols or keywords.  Returns a
  Clojure symbol.\n"
  (encode-scala-symbol "foo.++.bar.::") => 'foo.$plus$plus.bar.$colon$colon
  (encode-scala-symbol "++") => '$plus$plus
  (encode-scala-symbol '<init>) => '$lessinit$greater
  ;; trailing dots are removed
  (encode-scala-symbol "::.") => '$colon$colon)

^{:refer t6.from-scala.internal/safe-resolve :added "0.1.0"}
(fact "safe-resolve")

^{:refer t6.from-scala.types/if-let :added "0.2.0"}
(fact
 "Like `clojure.core/if-let` but with special handling of scala.Option.\n"
 ($/if-let [x ($/option 1)] (inc x) :no) => 2
 ($/if-let [x ($/option nil)] (inc x) :no) => :no)
