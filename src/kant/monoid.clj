(ns kant.monoid
  (:require [kant.impl.protocol :refer :all]
            [kant.impl.hierarchy :as h]))

(defn zero [m]
  (-zero m))

(defn plus [a & as]
  (cond
   (satisfies? MonoidSum a) (-sum a as)
   (satisfies? Monoid a) (reduce -plus a as)
   :else (throw (UnsupportedOperationException. "Monoid/-plus"))))
