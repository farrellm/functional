(ns kant.arrow
  (:require [clojure.core.match :refer [match]]
            [kant.monad.protocol :as p]
            [kant.hierarchy :as h]
            [kant.category :refer :all]
            [kant.applicative :refer :all]))

(defn -swap [[a b]] [b a])
(defn -dup  [a]   [a a])
(defn -flip [f]   #(f %2 %1))

;; Applicative
(defmethod pure+ ::h/arrow [m a]
  (p/-arr m (fn [_] a)))

(defmethod <*>+ ::h/arrow [af av]
  (>>> (p/-arr af -dup)  #(p/-first av %)
       (p/-arr af -swap) #(p/-first af %)
       (fn [[f v]] (f v))))

;; Arrow
(defmulti arr+
  (fn [a f] (h/most-general :arrow a)))

(defn arr [a f]
  (if (satisfies? p/Arrow a) (p/-arr a f)
      (arr+ a f)))

(defmulti arr-first+
  (fn [a f] (h/most-general :arrow a)))

(defn arr-first
  ([arr]   #(p/-first arr %))
  ([arr a] (if (satisfies? p/Arrow a) (p/-first arr a)
               (arr+ arr a))))

(defn arr-second
  ([arr]   #(arr-second arr %))
  ([arr p] (->> (-swap p) (p/-first arr) (-swap))))

(defn ***
  ([f g]   #(*** f g %))
  ([f g a] (->> a (arr-first f) (arr-second g))))

(defn &&&
  ([f g]   #(&&& f g %))
  ([f g a] (*** f g [a a])))
