(ns t6.from-scala.guide
  (:require [midje.sweet :refer :all]
            [t6.from-scala.core :refer ($ $$) :as $])
  (:import (scala.collection.immutable List)))

[[:chapter {:title "Introduction"}]]

"`from-scala` provides some syntactic sugar on top of Clojure's Java
interop support and some utility functions for directly interfacing
with Scala libraries from Clojure.  It uses a series of heuristics to
make calling Scala methods easier and less ugly."

[[:section {:title "Features"}]]
"
* Introduces a macro `$` for Scala interop, similar to the built-in
  special form `.`
  * It tries to resolve the correct Scala classes or companion objects
    without the need to use e.g. `List$/MODULE$` like with regular
    Java interop
  * Automatic encoding and decoding of method/class/object names that are
    invalid Java or Clojure identifiers (refer to the Scala class `::` as
    `\"::\"` in Clojure instead of `$colon$colon`)
  * It provides support for variable and default arguments
  * This is done at runtime using normal Java reflection
* `$/fn` provides Clojure-like syntax for creating Scala functions
* `$/tuple` creates Scala tuples
* `$/option` creates Scala options
* Provides views that make it easy to access and work with Scala
  collections, options, tuples and case classes (or any class that
  implements `scala.Product`)
* Using `$/view` Scala tuples and case classes can participate in
  normal destructuring
* Destructuring of Scala tuples, collections and case classes
"

[[:section {:title "Usage"}]]

"To use `from-scala` in your project, include this in your `project.clj`:"
(comment
  {:dependencies [...
                  [t6/from-scala "0.2.1-SNAPSHOT"]
                  [org.scala-lang/scala-library "2.11.5"]
                  ...]})

"You need to include a Scala library yourself.  `from-scala` works
with Scala 2.10 and 2.11. Choose the version you need or just copy
the example above."

[[:section {:title "Examples"}]]

"The features are best shown in a series of examples.  All examples
assume that you required `from-scala` like this in your namespace
declaration:"

(comment
  (ns your.ns
    (:require [from-scala.core :refer [$ $$] :as $])))

[[:subsection {:title "Creating an empty immutable Scala list"}]]

"#### With `from-scala`"

"Import the List class, so that it is available in the current namespace.
We do not need to import the companion object explicitly here!"
(import 'scala.collection.immutable.List)

"Call the companion object's empty method"
($ List/empty) ;; => #<Nil>

"#### Without `from-scala`"

"Import the List class' companion object (denoted by appending $ to List)"
(import 'scala.collection.immutable.List$)

"Call the method empty on the companion object"
(.empty List$/MODULE$) ;; => #<Nil>

[[:subsection {:title "Appending a new element to an empty list"}]]

"Create an empty list"
(def l ($ List/empty))

"#### With `from-scala`"

"Cons a 1 to it"
($ l "::" 1) ;; => #<$colon$colon List(1, Nil)>

"Here we called the method `::` on the List instance, which is actually
called `$colon$colon` in Java. We have to use a string here because
Clojure's reader can't read in the symbol `::`. The `$` macro
translates method names to the appropriate Java identifier. Method
names can be given as symbols or (constant) strings. You can still use
the symbol `$colon$colon` if you prefer."

"#### Without `from-scala`"

(.$colon$colon l 1)

"or using Clojure's `.` form"
(. l $colon$colon 1)

"Consing more than one element can be done with the help of the `$$`
macro:"

($$ l ("::" 1) ("::" 1))

"which expands to:"
(-> l ($ "::" 1) ($ "::" 2))

"Instead of all of the above you could also use `$/immutable-list` to
create the same list:"
($/immutable-list 1 2)

[[:subsection {:title "Creating a list of lists of integers"}]]

"First let us create a list of lists of integers:"
(def l ($ List & ($ List & 1 2 3) ($ List & 4 5 6)))

"`($ List ...)` calls the apply method on the `List` companion object.
`List`'s apply method takes variable arguments."

"Variable arguments are represented as `Seq`s in Scala. The `$` macro
provides syntax that automatically creates a `Seq` for you when you
need to call such a function. Everything after a `&` is added to the
`Seq`. Variable arguments can only appear at the end of a function's
signature."

"We are going to pass on showing how this can be done with regular
Clojure interop. It is way too ugly!

If you are curious anyway try macro expanding the call above."

"You can also do this explicitly by calling `$/var-args`:"
($ List ($/var-args 1 2 3))

[[:subsection {:title "Creating a Scala function"}]]

"Some Scala methods expect functions. `from-scala` provides a macro
that reifies a Scala function object with the appropriate arity for
you:"
($/fn [x] (inc x))

"It reifies the appropriate `scala.Function<n>` trait. In the example
we created a `scala.Function1` object that increments its argument."

"Once again macro expansion will show all the ugliness behind this."

[[:subsection {:title "Calling `flatMap` on a Scala collection"}]]

"We will reuse the list of lists of integers we created in the previous
examples."

"Call `flatMap` on the list, incrementing every number"
($ l flatMap
   ($/fn [xs]
     ($ xs map ($/fn [x] (inc x))
        ($ List/canBuildFrom)))
   ($ List/canBuildFrom))
;; => List(2, 3, 4, 5, 6, 7)

"`flatMap` takes an implicit parameter.  It expects a `CanBuildFrom`
instance.  You can create one for all Scala collection using
`($ <collection-type>/canBuildfrom)`.  `from-scala` does not resolve
implicits and you need to provide them yourself.  In the Scala
documentation you might have to look at *Full Signature* to see if a
method takes an implicit parameter."

[[:subsection {:title "Concatenating two Scala collections"}]]

(import 'scala.collection.immutable.Set)
(def s ($ Set & 1 2 3 4))
(def l ($ List & 1 5 2))
(def bf ($ Set/canBuildFrom))
($ s ++ l bf) ;; => Set(1, 2, 3, 4, 5)

"The `CanBuildFrom` instance determines what collection type you get
back."

[[:subsection {:title "Using default arguments"}]]

"There is no way to resolve the names of the arguments to a method at
runtime.  However we can determine which argument has a specified
default argument.  To signal that we want to use an argument's default
value we substitute it with an `_`."

"Assume the following Scala object is defined:
```scala
object MyObject {
  def apply(i: Int = 2, j: Int): Int = i + j
}
```"

"#### With `from-scala`"

"We can use the default value 2 for the first argument `i` with an
`_`:"
(comment
  (import 'MyObject)
  ($ MyObject _ 5) ;; => 7
  ($ MyObject 3 5) ;; => 8
  )

"#### Without `from-scala`"
(comment
  (import 'MyObject$)
  (.apply MyObject$/MODULE$ (.apply$default$1 MyObject$/MODULE$) 5) ;; => 7
  (.apply MyObject$/MODULE$ 3 5) ;; => 8
  )

[[:subsection {:title "Looping over Scala collections"}]]

"`from-scala` implements `cats`' `Monad`, `MonadZero`, `MonadPlus`, `Applicative`,
and `Functor` protocols for Scala iterables (i.e. all Scala collections)."

"This gets us for-like behavior using `mlet` for free.  `from-scala`
provides a wrapper for `mlet` called `$/for` which wraps the body in
an implicit `return`.  This makes `$/for` look like
`clojure.core/for`. It is however eager and not lazy."

"We also implement all of `cats` protocols for Scala's `Option` type.
So that you can use options with `$/for` just like you would in Scala
with `for`."

($/for [x ($ List & 1 2 3)
        y ($/immutable-list 4 5 6)]
   ($/tuple x y))
;; => List((1,4), (1,5), (1,6), (2,4), (2,5), (2,6), (3,4), (3,5), (3,6))

"Note that `$/for` does not return a lazy sequence like Clojure's
`for`.  It always returns the same container type as the first
binding."

"`$/tuple` returns a `scala.TupleN` instance, where `N` is the number
of elements in the tuple with `1 <= N <= 22`."

[[:subsection {:title "Partial application of Scala functions"}]]

"Use `$/partial` if you need to partially apply arguments to a Scala
function."

(def plus ($/fn [a b] (+ a b)))
($ plus apply 4 5) ;; => 9

(def plus5 ($/partial plus 5))
($ plus5 apply 3) ;; => 8

[[:subsection {:title "Creating a `scala.Option`"}]]

(import 'scala.Option)

($ Option :value) ;; => #<Some :value>

($ Option nil) ;; => #<None$>

"Prefer using `Option` rather than using `Some` directly. It looks
nicer and works well with Clojure's
`nil`-returning-functions. Alternatively there is wrapper called
`$/option`."

"Unpack the `Option` with `$/view`. You can then use `if-let` or
`when-let` like you normally would in Clojure:"

(def my-option ($/option :value))
(if-let [v ($/view my-option)]
  (str "option has Some(" v ")")
  "option was None$")
;; => "option has Some(:value)"

"Alternatively use the option's `getOrElse` method, however you need to
wrap the default value for `getOrElse` in a Scala function."

($ my-option getOrElse ($/fn [] :default-value))

"Or use `$/if-let` which implicitly calls `$/view` on the value:"
(if-let [v my-option]
  (str "option has Some(" v ")")
  "option was None$")
;; => "option has Some(:value)"

"This is safe even if the value is not a Scala option. Note that the
above expression will print `\"option was None$\"` if the value is a
`None$` or if it is `nil`. It makes no distinction between those two
values. Keep this in mind when you use `$/if-let` and `$/view`."

[[:section {:title "License"}]]

"Copyright (c) 2014-2015 Tobias Kortkamp

Distributed under the terms of the MIT License.
"

[[:file {:src "test/t6/from_scala/internal_test.clj"}]]
[[:file {:src "test/t6/from_scala/utils_test.clj"}]]
