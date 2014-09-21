(ns core.monad
  (:require [clojure.core.match :refer [match]]))

(defprotocol Functor
  (-fmap [v f]))
;; order of parameters in protocol is necessarily reversed since
;; dispatch is on the first argument.
(defn fmap [f v] (-fmap v f))

(defprotocol Applicative
  (pure [_ val] "constructor")
  (-<*> [a b] "application"))

(declare <*)
(defn <*>
  ([f] (fmap #(%) f))
  ([f a & r] (if r (apply <*> (<* f a) r)
                 (-<*> f a))))
(defn <*
  "partial application in an applicative"
  ([af] af)
  ([af a & r] (if r (apply <* (<* af a) r)
                  (<*> (fmap (fn [f] #(partial f %)) af) a))))

(defprotocol Monad
  (>>= [m f] "bind"))

(defn m-do* [body]
  (match [body]
     [([val] :seq)] val
     [([fst & rst] :seq)] (match fst
                             [:let & vs] `(let [~@vs] ~(m-do* rst))
                             [k v] `(>>= ~v (fn [~k] ~(m-do* rst)))
                             v     `(>>= ~v (fn [_#] ~(m-do* rst))))))

(defmacro m-do [& body]
  (m-do* body))

(defn applicative-fmap [v f]
  (<*> (pure v f) v))

(defn monad-<*> [af av]
  (m-do [f af]
        [v av]
        (pure af (f v))))

;; (extend-type Object
;;   Functor
;;   (-fmap [v f] (applicative-fmap v f))
;;   Applicative
;;   (<*> [af av] (monad-<*> af av)))

(extend-type clojure.lang.Sequential
  Functor
  (-fmap [v f] #_(into []) (map f v))

  Applicative
  (pure [_ v] [v])
  (-<*>
    ([f v] (mapcat #(map % v) f)))

  Monad
  (>>= [m f] (mapcat f m)))

(defprotocol Just
  (value [m] "extract value"))
(defn just [v]
  (reify
    Object
    (toString [_] (str "Just " v))

    Functor
    (-fmap [_ f] (just (f v)))

    Applicative
    (pure [_ u] (just u))
    (-<*> [_ m] (just (v (value m))))
    
    Monad
    (>>=    [_ f] (f v))

    Just
    (value [_] v)))

(def nothing
  (reify
    Object
    (toString [_] "Nothing")

    Functor
    (-fmap [_ f] nothing)

    Applicative
    (pure [_ u] (just u))
    (-<*>  [_ f] nothing)

    Monad
    (>>= [_ f] nothing)))
