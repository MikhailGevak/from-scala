(ns t6.from-scala.guide
  (:require [midje.sweet :refer :all]
            [t6.from-scala.core :refer ($ $$) :as $])
  (:import (scala.collection.immutable List)))

[[:chapter {:title "Introduction"}]]

"`from-scala` provides some syntactic sugar on top of Clojure's Java interop support and some utility functions for directly interfacing with Scala libraries from Clojure. It uses a couple of heuristics to make calling Scala methods easier."

[[:section {:title "Features"}]]
"
 * Automatic resolving of companion objects for classes
 * Automatic encoding and decoding of method/class/object names that are invalid Java or Clojure identifiers (refer to the Scala class `::` as `\"::\"` in Clojure instead of `$colon$colon`)
 * Creating Scala functions and tuples from Clojure
 * Destructuring of Scala tuples, collections and case classes
"

[[:section {:title "Usage"}]]

"To use `from-scala` in your project, include this in your `project.clj`:"
(comment
  {:dependencies [...
                  [t6/from-scala "0.2.0"]
                  [org.scala-lang/scala-library "2.11.5"]
                  ...]})

"You need to include a Scala library yourself. `from-scala` works
with Scala 2.10 and 2.11. Choose the version you need or just copy
the example above."

"All examples assume that you imported `from-scala` like this:"

(comment
  (ns your.ns
    ...
    (:require [t6.from-scala.core :refer ($ $$) :as $]
              ...)
    (:import scala.collection.immutable.List
             scala.collection.immutable.Set)
    ...))

[[:section {:title "Example"}]]

"An example of using Scala's collection API with `from-scala`:"
(fact
  ($ ($ List & 1 2 3 4)
     reduce
     ($/fn [x y] (+ x y)))) => 10

[[:section {:title "License"}]]

"Copyright (c) 2014 Tobias Kortkamp

Distributed under the terms of the MIT License.
"

[[:file {:src "test/t6/from_scala/core_test.clj"}]]
