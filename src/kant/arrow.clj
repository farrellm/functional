(ns kant.arrow
  (:require [kant.monad.protocol :as p]
            [kant.hierarchy :as h]
            [kant.category :refer :all]
            [kant.applicative :refer :all]))

(declare &&&)

(defn -swap [[a b]] [b a])
(defn -dup  [a]   [a a])
(defn -flip [f]   #(f %2 %1))

(defn $
  "function application operator"
  [f & args]
  (apply f args))

;; Arrow
(defmulti arr+
  (fn [a f] (h/most-general :arrow a)))

(defn arr [a f]
  (if (satisfies? p/Arrow a)
    (p/-arr a f)
    (arr+ a f)))

(defmulti arr-first+
  (fn [a] (h/most-general :arrow a)))

(defn arr-first [arr]
  (if (satisfies? p/Arrow arr)
    (p/-first arr)
    (arr-first+ arr)))

;; Applicative
(defmethod pure+ ::h/arrow [m a]
  (arr m (fn [_] a)))

(defmethod <*>+ ::h/arrow [af av]
  (>>> (&&& af av)
       (arr af (fn [[f v]] (f v)))))

;; other arrow functions
(defn arr-second [a]
  (if (satisfies? p/ArrowSecond a)
    (p/-second a)
    (>>> (arr a -swap) (arr-first a) (arr a -swap))))

;; clojure complains about ***
(defn xxx [f g]
  (>>> (arr-first f) (arr-second g)))

(defn &&& [f g]
  (>>> (arr f -dup) (xxx f g)))
