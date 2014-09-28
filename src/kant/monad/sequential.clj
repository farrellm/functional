(ns kant.monad.sequential
  (:require [kant.monad.protocol :refer :all]))

(extend-type clojure.lang.Sequential
  Functor
  (-fmap [v f] #_(into []) (map f v))

  Pure
  (-pure [_ v] [v])

  Applicative
  (-ap  [f v] (mapcat #(map % v) f))

  Monad
  (-bind [m f] (mapcat f m))

  Monoid
  (-zero [_] [])
  (-plus [a b] (concat a b))

  MonoidSum
  (-sum [a as] (apply concat a as)))
