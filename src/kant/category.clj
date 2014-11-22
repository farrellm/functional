(ns kant.category
  (:require [clojure.core.match :refer [match]]
            [kant.impl.protocol :as p]
            [kant.impl.hierarchy :as h]))

;; Category
(defmulti id+
  (fn [c] (h/most-general :category c)))

(defn id [c]
  (if (satisfies? p/Category c) (p/-id c)
      (id+ c)))

(defmulti comp+
  (fn [a b] (h/most-general :category a)))

(defn compose [a b]
  (if (satisfies? p/Category a) (p/-comp a b)
      (comp+ a b)))

(defn <<<
  ([a] a)
  ([a b & c] (if c (compose a (apply <<< b c))
                 (compose a b))))

(defn >>>
  ([a] a)
  ([a b & c] (if c (compose (apply >>> b c) a)
                 (compose b a))))
