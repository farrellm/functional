(ns functional.monad.maybe-t
  (:require [functional.monad :refer :all]
            [functional.monad.maybe :refer :all]
            [functional.monad.sequential :refer :all]
            [functional.monad.maybe-t :refer :all]))

(defprotocol MaybeT
  (run-maybe [_]))
(defn maybe-t [v]
  (reify
    Object
    (equals [_ o] (and (satisfies? MaybeT o)
                       (= v (run-maybe o))))
    (toString [_] (str "MaybeT " v))
    
    MaybeT
    (run-maybe [_] v)
    
    Pure
    (pure [_ u] (maybe-t (pure v (just u))))
    
    Monad
    (-bind [_ g] (maybe-t (>>= v #(maybe (pure v nothing)
                                         (comp run-maybe g) %))))))

(require '[functional.monad.sequential :refer :all])

(def mv (maybe-t [(just 1) nothing (just 2)]))

(run-maybe (>>= mv #(maybe-t [(just (inc %))])))
