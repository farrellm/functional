(ns clojure.core.monad.maybe
  (:require [clojure.core.monad :refer :all]))

(defprotocol Just
  (value [m] "extract value"))
(defn just [v]
  (reify
    Object
    (equals [_ o] (and (satisfies? Just o) (= v (value o))))
    (toString [_] (str "Just " v))

    Functor
    (-fmap [_ f] (just (f v)))

    Applicative
    (pure [_ u] (just u))
    (-ap  [_ m] (just (v (value m))))
    
    Monad
    (-bind [_ f] (f v))

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
    (-ap  [_ f] nothing)

    Monad
    (-bind [_ f] nothing)))
