(ns kant.arrow.kleisli
  (:require [kant.monad :refer :all]
            [kant.monad.protocol :refer :all]))

(defprotocol Kleisli
  (run-kleisli [_]))
(defn kleisli [f]
  (reify
    Kleisli
    (run-kleisli [_] f)
    
    Category
    ;; (id [_] (kleisli #[:return %]))
    (-comp [_ b] (>=> (run-kleisli b) f))

    ;; Should implement Pure, but needs return type polymorphism
    ;; (pure [g] (kleisli (comp #[:return %] g)))

    Arrow
    (-first [_ m] (m-do [[a b] m
                         c (f a)]
                        [:return [c b]]))))
