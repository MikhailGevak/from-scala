(ns t6.from-scala.core
  (:refer-clojure :exclude [partial for if-let fn])
  (:require [potemkin :refer (import-vars import-macro import-fn)]
            t6.from-scala.internal
            t6.from-scala.utils))

(import-vars t6.from-scala.internal/$
             t6.from-scala.internal/$$
             t6.from-scala.internal/fn
             t6.from-scala.internal/var-args
             t6.from-scala.internal/decode-scala-symbol
             t6.from-scala.internal/encode-scala-symbol

             t6.from-scala.utils/for
             t6.from-scala.utils/if-let
             t6.from-scala.utils/immutable-list
             t6.from-scala.utils/immutable-map
             t6.from-scala.utils/immutable-set
             t6.from-scala.utils/immutable-vector
             t6.from-scala.utils/option
             t6.from-scala.utils/partial
             t6.from-scala.utils/tuple
             t6.from-scala.utils/view
             t6.from-scala.utils/execution-context)

(import-macro t6.from-scala.internal/fn function)
(import-fn t6.from-scala.utils/wrap-fn wrap-function)
