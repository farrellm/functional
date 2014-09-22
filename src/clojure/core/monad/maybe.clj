(ns clojure.core.monad.maybe
  (:require [clojure.core.monad :refer :all]))

(defprotocol Just
  (value [m] "extract value"))
(defn just [v]
  (reify
    Object
    (toString [_] (str "Just " v))

    Functor
    (-fmap [_ f] (just (f v)))

    Applicative
    (pure [_ u] (just u))
    (-<*> [_ m] (just (v (value m))))
    
    Monad
    (>>=    [_ f] (f v))

    Just
    (value [_] v)))

(def nothing
  (reify
    Object
    (toString [_] "Nothing")

    Functor
    (-fmap [_ f] nothing)

    Applicative
    (pure [_ u] (just u))
    (-<*>  [_ f] nothing)

    Monad
    (>>= [_ f] nothing)))
