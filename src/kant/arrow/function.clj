(ns kant.arrow.function
  (:require [kant.monad.protocol :refer :all]))

(extend-type clojure.lang.IFn
  Category
  (-id [_] identity)
  (-comp [a b] (comp a b))

  Pure
  (-pure [_ a] (fn [_] a))

  Arrow
  (-first [f [a b]] [(f a) b]))
