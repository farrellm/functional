(ns kant.arrow.kleisli
  (:require [kant.monad :refer :all]
            [kant.monad.protocol :refer :all]))

(defprotocol Kleisli
  (run-kleisli [_]))
(defn kleisli [f]
  (reify
    Kleisli
    (run-kleisli [_] f)

    clojure.lang.IFn
    (invoke [_ a] (f a))
    
    Category
    ;; (id [_] (kleisli #[:return %]))
    (-comp [_ b] (>=> (run-kleisli b) f))

    Arrow
    (-arr [_ f] (kleisli f))
    (-first [_ m] (m-do [[a b] m
                         c (f a)]
                        [:return [c b]]))))
