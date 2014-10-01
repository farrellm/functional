(ns kant.functor
  (:require [kant.monad.protocol :refer :all]
            [kant.hierarchy :as h]))

(defmulti fmap+
  (fn [f v] (h/most-general :functor v)))

(defn fmap [f v]
  (if (satisfies? Functor v) (-fmap v f)
      (fmap+ f v)))
