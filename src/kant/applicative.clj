(ns kant.applicative
  (:require [kant.monad.protocol :refer :all]
            [kant.functor :refer :all]
            [kant.hierarchy :as h]))

;; Pure
(defn pure [m a]
  (cond
   (satisfies? Pure m) (-pure m a)
   (satisfies? Arrow m) (-arr m (fn [_] a))
   :else (throw (UnsupportedOperationException. "Pure/-pure"))))

;; Applicative
(declare <*)

(defmulti <*>
  (fn [af & _] (h/most-general :applicative af)))

(defmethod <*> ::h/applicative
  ([af] (fmap #(%) af))
  ([af av & avs] (if avs (apply <*> (<* af av) avs)
                     (-ap af av))))

(defn <*
  "partial application in an applicative"
  ([af] af)
  ([af a & r] (if r (apply <* (<* af a) r)
                  (<*> (fmap (fn [f] #(partial f %)) af) a))))

(defn m-sequence [[a & as]]
  (apply <*> (pure a vector) a as))

(defmethod fmap ::h/applicative [f v]
  (<*> (pure v f) v))
