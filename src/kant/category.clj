(ns kant.category
  (:require [clojure.core.match :refer [match]]
            [kant.monad.protocol :as p]
            [kant.hierarchy :as h]))

;; Category
(defmulti id+
  (fn [c] (h/most-general :category c)))

(defn id [c]
  (if (satisfies? p/Category c) (p/-id c)
      (id+ c)))

(defn <<<
  ([a] a)
  ([a b & c] (if c (p/-comp a (apply <<< b c))
                 (p/-comp a b))))

(defn >>>
  ([a] a)
  ([a b & c] (if c (p/-comp (apply >>> b c) a)
                 (p/-comp b a))))
