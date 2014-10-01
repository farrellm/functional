(ns kant.category
  (:require [clojure.core.match :refer [match]]
            [kant.monad.protocol :refer :all]
            [kant.hierarchy :as h]))

;; Category
(defn <<<
  ([a] a)
  ([a b & c] (if c (-comp a (apply <<< b c))
                 (-comp a b))))

(defn >>>
  ([a] a)
  ([a b & c] (if c (-comp (apply >>> b c) a)
                 (-comp b a))))
