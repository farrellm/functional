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
    #_([f]   #_(into []) (map #(%) f))
    ([f v] #_(into []) (mapcat #(map % v) f))
    #_([f a b & c] (if c :a #_(apply <*> (<* f a) b c)
                     :b #_(<*> (<* f a) b))))

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
    (<*> [_ m] (just (v (value m))))
    
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
    (<*>  [_ f] nothing)

    Monad
    (>>= [_ f] nothing)))


(m-do* '((just 8)))
(m-do* '([x (just 1)]
         (just 1)
         [:let y 5]
         (just y)))

(m-do [x (just 1)]
      (just 2)
      [:let y 5
            z 3]
      (just [x y z]))


(just 8)
(fmap inc (just 8))
(<*> (just inc) (just 8))

(>>= (just 8) #(just [%1 %1]))
(>>= nothing #(just [%1 %1]))

(fmap inc [1 2 3])
(pure [] 8)
(<*> (pure [] inc) [1 2 3])
(>>= [1 2] #(vector %1 %1))

(<*> [#(identity 9)])
(<*> (<* [identity] [9]))
(<*> (<* [inc] [8]))

(fmap inc [8])
(<*> [inc] [8])

(<*> (<* (<* [+] [0 10] [0 100]) [1 1] [7]))

(<*> [+])
(<*> [+] [1])
(<*> [+] [1] [2])
(<*> [+] [1] [1 2] [1 2 3])
