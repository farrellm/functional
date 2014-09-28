(ns functional.monad.either
  (:require [functional.monad.protocol :refer :all]
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

    Pure
    (-pure [_ u] (right u))

    Monad
    (-bind [_ f] (f v))

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

    Pure
    (-pure [_ u] (right u))

    Monad
    (-bind [m f] m)

    clojure.core.match.protocols/IMatchLookup
    (val-at [_ k not-found]
      (case k
        :left v
        not-found))))

;; (fmap inc (right 8))
;; (<*> (right inc) (right 8))
;; (>>= (right 8) #(right (inc %)))
