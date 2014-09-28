(ns kant.arrow.function
  (:require [kant.monad.protocol :refer :all]))

(defprotocol Function
  (run-function [_]))
(defn function [f]
  (reify
    Function
    (run-function [_] f)
    
    Category
    (-id [_] (function identity))
    (-comp [_ b] (function (comp f (run-function b))))

    Pure
    (-pure [_ g]
      (function g))

    Arrow
    (-first
      [_ [a b]] [(f a) b])))
