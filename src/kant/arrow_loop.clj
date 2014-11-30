(ns kant.arrow-loop
  (:require [kant.impl.protocol :refer :all]
            [kant.impl.hierarchy :as h]))

(defn arr-loop [m]
  (-loop m))
