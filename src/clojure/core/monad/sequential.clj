(ns clojure.core.monad.sequential
  (:require [clojure.core.monad :refer :all]))

(extend-type clojure.lang.Sequential
  Functor
  (-fmap [v f] #_(into []) (map f v))

  Applicative
  (pure [_ v] [v])
  (-<*>
    ([f v] (mapcat #(map % v) f)))

  Monad
  (>>= [m f] (mapcat f m))

  Monoid
  (mempty [_] [])
  (mappend [a b] (concat a b))

  MonoidConcat
  (-mconcat [a as] (apply concat a as)))
