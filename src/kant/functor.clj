(ns kant.functor
  (:require [kant.monad.protocol :refer :all]
            [kant.hierarchy :as h]))

(defmulti fmap
  (fn [f v] (h/most-general :functor v)))

(defmethod fmap ::h/functor [f v]
  (-fmap v f))
