(ns t6.from-scala.core
  (:refer-clojure :exclude [partial for if-let fn])
  (:require [potemkin :refer (import-vars import-macro import-fn)]
            t6.from-scala.internal
            t6.from-scala.types))

(import-vars t6.from-scala.internal/$
             t6.from-scala.internal/$$
             t6.from-scala.internal/fn
             t6.from-scala.internal/var-args
             t6.from-scala.internal/decode-scala-symbol
             t6.from-scala.internal/encode-scala-symbol

             t6.from-scala.types/for
             t6.from-scala.types/if-let
             t6.from-scala.types/immutable-list
             t6.from-scala.types/immutable-map
             t6.from-scala.types/immutable-set
             t6.from-scala.types/immutable-vector
             t6.from-scala.types/option
             t6.from-scala.types/partial
             t6.from-scala.types/tuple
             t6.from-scala.types/view
             t6.from-scala.types/wrap-fn)

(import-macro t6.from-scala.internal/fn function)
(import-fn t6.from-scala.types/wrap-fn wrap-function)
