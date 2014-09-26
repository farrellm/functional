(ns functional.arrow.kleisli
  (:require [functional.monad :refer :all]))

(defprotocol Kleisli
  (run-kleisli [_]))
(defn kleisli [f]
  (reify
    Kleisli
    (run-kleisli [_] f)
    
    Category
    (id [_] (kleisli pure))
    (-comp [_ b] (>=> (run-kleisli b) f))

    Arrow
    (-first [_ m] (m-do [[a b] m
                         c (f a)]
                        [:return [c b]]))))
