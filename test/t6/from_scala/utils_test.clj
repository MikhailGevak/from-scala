(ns t6.from-scala.utils-test
  (:refer-clojure :exclude [for partial])
  (:use midje.sweet)
  (:require [t6.from-scala.core :refer ($ $$) :as $])
  (:import (scala.collection.immutable List Set)
           (clojure.lang ExceptionInfo)))

(defchecker instance-of [expected-class]
  (checker [object] (instance? expected-class object)))

[[:chapter {:title "Utility functions"}]]

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

^{:refer t6.from-scala.types/if-let :added "0.2.0"}
(fact
 "Like `clojure.core/if-let` but with special handling of scala.Option.\n"
 ($/if-let [x ($/option 1)] (inc x) :no) => 2
 ($/if-let [x ($/option nil)] (inc x) :no) => :no)
