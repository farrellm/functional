(ns kant.arrow-choice
  (:require [clojure.core.match :refer [match]]
            [kant.monad.protocol :as p]
            [kant.hierarchy :as h]
            [kant.category :refer :all]
            [kant.arrow :refer :all]
            [kant.monad.either :as e]))

;; ArrowChoice
(defmulti left+
  (fn [a] (h/most-general :arrow-choice a)))

(defn left [arr]
  (if (satisfies? p/ArrowChoice arr) (p/-left arr)
      (left+ arr)))

(defmulti right+
  (fn [a] (h/most-general :arrow-choice a)))

(defn right [arr]
  (if (satisfies? p/ArrowChoice arr) (p/-right arr)
      (right+ arr)))

(defn +++ [f g]
  (>>> (left f) (right g)))

(defn ||| [f g]
  (>>> (+++ f g) (arr f #(match [%]
                           [{:left v}] v
                           [{:right v}] v))))
