(ns kant.applicative
  (:require [kant.monad.protocol :refer :all]
            [kant.functor :refer :all]
            [kant.hierarchy :as h]))

;; Pure
(defmulti pure+
  (fn [a v] (h/most-general :applicative a)))

(defn pure [m a]
  (if (satisfies? Pure m) (-pure m a)
      (pure+ m a)))

;; Applicative
(declare <*)

(defmulti <*>+
  (fn [af & _] (h/most-general :applicative af)))

(defn <*>
  ([af] (fmap #(%) af))
  ([af av & avs] (cond
                  avs (apply <*> (<* af av) avs)
                  (satisfies? Applicative af) (-ap af av)
                  :else (<*>+ af av))))

(defn <*
  "partial application in an applicative"
  ([af] af)
  ([af a & r] (if r (apply <* (<* af a) r)
                  (<*> (fmap (fn [f] #(partial f %)) af) a))))

(defn m-sequence [[a & as]]
  (apply <*> (pure a vector) a as))

;; Functor
(defmethod fmap+ ::h/applicative [f v]
  (<*> (pure v f) v))
