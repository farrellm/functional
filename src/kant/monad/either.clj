(ns kant.monad.either
  (:require [kant.impl.protocol :as p]
            [clojure.core.match :refer [match]]))

(defprotocol Either)
(defprotocol Left
  (-left [m]))
(defprotocol Right
  (-right [m]))

(defn right [v]
  (reify
    Either
    Right
    (-right [_] v)

    Object
    (equals [_ o] (and (satisfies? Right o) (= v (-right o))))
    (toString [_] (str "Right " v))

    p/Pure
    (p/-pure [_ u] (right u))

    p/Monad
    (p/-bind [_ f] (f v))

    clojure.core.match.protocols/IMatchLookup
    (val-at [_ k not-found]
      (case k
        :right v
        not-found))))

(defn left [v]
  (reify
    Either
    Left
    (-left [_] v)

    Object
    (equals [_ o] (and (satisfies? Left o) (= v (-left o))))
    (toString [_] (str "Left " v))

    p/Pure
    (p/-pure [_ u] (right u))

    p/Monad
    (p/-bind [m f] m)

    clojure.core.match.protocols/IMatchLookup
    (val-at [_ k not-found]
      (case k
        :left v
        not-found))))

(defn either [f g e]
  (match [e]
    [{:left v}]  (f v)
    [{:right v}] (g v)))

(defn mirror [e]
  (match [e]
    [{:left v}]  (right v)
    [{:right v}] (left v)))

;; (fmap inc (right 8))
;; (<*> (right inc) (right 8))
;; (>>= (right 8) #(right (inc %)))
