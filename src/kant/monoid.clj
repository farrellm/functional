(ns kant.monoid
  (:require [kant.monad.protocol :refer :all]
            [kant.hierarchy :as h]))

(defn zero [m]
  (-zero m))

(defn plus [a & as]
  (cond
   (satisfies? MonoidSum a) (-sum a as)
   (satisfies? Monoid a) (reduce -plus a as)
   :else (throw (UnsupportedOperationException. "Monoid/-plus"))))
