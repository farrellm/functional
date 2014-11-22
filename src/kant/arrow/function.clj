(ns kant.arrow.function
  (:require [kant.monad.protocol :refer :all]))

(extend-type clojure.lang.AFunction
  Category
  (-id [_] identity)
  (-comp [a b] (comp a b))

  Pure
  (-pure [_ a] (fn [_] a))

  Arrow
  (-arr [_ f] f)
  (-first [f] (fn [[a b]] [(f a) b])))
