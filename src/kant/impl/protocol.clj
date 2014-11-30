(ns kant.impl.protocol)

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
  (-arr [_ f]))
(defprotocol ArrowFirst
  (-first [a]))
(defprotocol ArrowSecond
  "Optional protocol for more efficient second"
  (-second [a]))

(defprotocol ArrowChoice
  (-left [a]))
(defprotocol ArrowChoiceRight
  "Optional protocol for more efficient right"
  (-right [a]))

(defprotocol ArrowApply
  (-app [_]))

(defprotocol ArrowLoop
  (-loop [_]))

(defprotocol Monad
  (-bind [m f]))

(defprotocol Monoid
  (-zero [_])
  (-plus [a b]))
(defprotocol MonoidSum
  "Optional protocol for more efficient sum"
  (-sum [a as]))
