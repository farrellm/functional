(ns kant.monad.function
  (:require [kant.impl.protocol :as p]))

(extend-type clojure.lang.AFunction
  p/Category
  (-id [_] identity)
  (-comp [a b] (comp a b))

  p/Pure
  (-pure [_ a] (fn [_] a))

  p/Arrow
  (-arr [_ f] f)

  p/ArrowChoice
  (-left [f] (partial e/either (comp e/left f) (comp e/right identity)))

  p/ArrowApply
  (-app [_] (fn [[f a]] (f a))))
