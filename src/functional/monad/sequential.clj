(ns functional.monad.sequential
  (:require [functional.monad :refer :all]))

(extend-type clojure.lang.Sequential
  Functor
  (-fmap [v f] #_(into []) (map f v))

  Applicative
  (pure [_ v] [v])
  (-ap  [f v] (mapcat #(map % v) f))

  Monad
  (-bind [m f] (mapcat f m))

  Monoid
  (m-zero [_] [])
  (-m-plus [a b] (concat a b))

  MonoidSum
  (-m-sum [a as] (apply concat a as)))
