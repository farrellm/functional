(ns functional.monad.state
  (:require [functional.monad :refer :all])
  (:require [clojure.core.match :refer [match]]))

(defprotocol State
  (run-state [f]))

(defn state [f]
  (reify
    State
    (run-state [_] f)

    Pure
    (pure [_ v]
      (state (fn [s] [v s])))

    Monad
    (-bind [_ g] (state (fn [s0]
                          (let [[a s1] (f s0)]
                            ((run-state (g a)) s1)))))))

(defn put [v]
  (state (fn [s] [nil v])))

(defn gets
  ([]  (state (fn [s] [s s])))
  ([f] (state (fn [s] [(f s) s]))))

(defn modify [f]
  (state (fn [s] [nil (f s)])))
