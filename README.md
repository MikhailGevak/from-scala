# from-scala

[![Build Status](https://travis-ci.org/t6/from-scala.svg?branch=master)](https://travis-ci.org/t6/from-scala)

`from-scala` provides some syntactic sugar on top of Clojure's Java interop support and some utility functions for directly interfacing with Scala libraries from Clojure. It uses a couple of heuristics to make calling Scala methods easier.

## Features

 * Automatic resolving of companion objects for classes
 * Automatic encoding and decoding of method/class/object names that are invalid Java or Clojure identifiers (refer to the Scala class `::` as `"::"` in Clojure instead of `$colon$colon`)
 * Creating Scala functions and tuples from Clojure
 * Destructuring of Scala tuples, collections and case classes

## Usage

To use `from-scala` in your project, include this in your `project.clj`:
```clojure
{:dependencies [...
                [t6/from-scala "0.2.0"]
                [org.scala-lang/scala-library "2.11.5"]
                ...]}
```

You need to include a Scala library yourself. `from-scala` works
with Scala 2.10 and 2.11. Choose the version you need or just copy
the example above.

All examples assume that you imported `from-scala` like this:
```clojure
(ns your.ns
  ...
  (:require [t6.from-scala.core :refer ($ $$) :as $]
            ...)
  ...))
```

* [Documentation for the latest release](https://t6.github.io/from-scala/0.2.0/)
* [Documentation for the current snapshot](https://t6.github.io/from-scala/)

## Example

An example of using Scala's collection API with `from-scala`:
```clojure
($ ($ List & 1 2 3 4)
   reduce
   ($/function [x y] (+ x y))))
;; => 10
```

## License

Copyright © 2014 Tobias Kortkamp

Distributed under the terms of the MIT License.
