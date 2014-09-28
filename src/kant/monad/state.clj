(ns kant.monad.state
  (:require [kant.monad.protocol :refer :all]))

(defprotocol State
  (run-state [f]))

(defn state [f]
  (reify
    State
    (run-state [_] f)

    Pure
    (-pure [_ v]
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
