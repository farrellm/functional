(ns kant.monad.protocol)

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
  (-arr [_ f])
  (-first [a]))

(defprotocol ArrowChoice
  (-left [a])
  (-right [a]))

(defprotocol Monad
  (-bind [m f]))

(defprotocol Monoid
  (-zero [_])
  (-plus [a b]))
(defprotocol MonoidSum
  "Optional protocol for more efficient sum"
  (-sum [a as]))
