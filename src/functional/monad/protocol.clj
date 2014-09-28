(ns functional.monad.protocol)

(defprotocol Functor
  (-fmap [v f]))

(defprotocol Pure
  (-pure [_ val]))

(defprotocol Applicative
  (-ap [a b]))

(defprotocol Category
  (-id [_])
  (-comp [_ a]))

(defprotocol Arrow
  ;; (-arr)
  (-first [_ a]))

(defprotocol Monad
  (-bind [m f] "bind"))

(defprotocol Monoid
  (-zero [_])
  (-plus [a b]))
(defprotocol MonoidSum
  "Optional protocol for more efficient sum"
  (-sum [a as]))
