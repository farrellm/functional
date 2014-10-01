(ns kant.arrow
  (:require [clojure.core.match :refer [match]]
            [kant.monad.protocol :refer :all]
            [kant.hierarchy :as h]
            [kant.category :refer :all]
            [kant.applicative :refer :all]))

(defn -swap [[a b]] [b a])
(defn -dup  [a]   [a a])
(defn -flip [f]   #(f %2 %1))

;; Applicative
(defmethod <*> ::h/arrow [af av]
  (>>> (-arr af -dup)  #(-first av %)
       (-arr af -swap) #(-first af %)
       (fn [[f v]] (f v))))

(defn arr [a f]
  (-arr a f))

(defn arr-first
  ([arr]   #(-first arr %))
  ([arr a] (-first arr a)))

(defn arr-second
  ([arr]   #(arr-second arr %))
  ([arr p] (->> (-swap p) (-first arr) (-swap))))

(defn ***
  ([f g]   #(*** f g %))
  ([f g p] (->> p (arr-first f) (arr-second g))))

(defn &&&
  ([f g]   #(&&& f g %))
  ([f g a] (*** f g [a a])))
