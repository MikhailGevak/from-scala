(ns t6.from-scala.internal-test
  (:use midje.sweet)
  (:require [t6.from-scala.core :as $ :refer [$ $$]])
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

^{:refer t6.from-scala.internal/decode-scala-symbol :added "0.1.0"}
(fact
 "Transforms a Java identifier to a Scala identifier. Accepts
  strings, symbols or keywords. Returns a string.\n"
 ($/decode-scala-symbol "foo.$plus$plus.bar.$colon$colon") => "foo.++.bar.::"
 ($/decode-scala-symbol "$plus$plus") => "++"
 ($/decode-scala-symbol '$lessinit$greater) => "<init>")

^{:refer t6.from-scala.internal/encode-scala-symbol :added "0.1.0"}
(fact
 "Transforms a Scala identifier (e.g. method or class names) to a
  Java identifier. Accepts strings, symbols or keywords.  Returns a
  Clojure symbol.\n"
 ($/encode-scala-symbol "foo.++.bar.::") => 'foo.$plus$plus.bar.$colon$colon
 ($/encode-scala-symbol "++") => '$plus$plus
 ($/encode-scala-symbol '<init>) => '$lessinit$greater
 ;; trailing dots are removed
 ($/encode-scala-symbol "::.") => '$colon$colon)
