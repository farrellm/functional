(ns kant.monad.maybe-t
  (:require [kant.monad :refer :all]
            [kant.monad.protocol :refer :all]
            [kant.monad.maybe :refer :all]))

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
    (-pure [_ u] (maybe-t (pure v (just u))))
    
    Monad
    (-bind [_ g] (maybe-t (>>= v #(maybe (pure v nothing)
                                         (comp run-maybe g) %))))))
