(ns kant.arrow-apply
  (:require [kant.impl.protocol :as p]
            [kant.impl.hierarchy :as h]
            [kant.category :refer :all]
            [kant.arrow :refer :all]
            [kant.arrow-choice :refer :all]
            [kant.applicative :refer :all]
            [kant.monad :refer :all]
            [kant.monad.either :as e]
            [clojure.core.match :refer [match]]))

;; ArrowApply
(defmulti app+
  (fn [a] (h/most-general :arrow-apply a)))

(defn app [arr]
  (if (satisfies? p/ArrowApply arr)
    (p/-app arr)
    (app+ arr)))

;; ArrowChoice
(defmethod left+ ::h/arrow-apply [a]
  (arr a #(e/either )))

;; Monad
(defmethod >>=+ ::h/arrow-apply [m f]
  #(let [a (m %)
         g (f a)]
     (g %)))

;; Applicative
(prefer-method pure+ ::h/arrow ::h/monad)
(prefer-method <*>+ ::h/arrow ::h/monad)
