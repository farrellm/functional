(ns core.monad
  (:require [clojure.core.match :refer [match]]))

(defprotocol Monad
  (>>= [m f] "bind")
  (return [_ val] "constructor"))

(extend clojure.lang.PersistentVector
  Monad
  {:>>=    (fn [m f] (mapcat f m))
   :return (fn [_ v] [v])})

(defprotocol Just
  (value [m] "extract value"))
(defn just [v]
  (reify
    Object
    (toString [_] (str "Just " v))
    
    Monad
    (>>=    [_ f] (f v))
    (return [_ u] (just u))

    Just
    (value [_] v)))

(def nothing
  (reify
    Object
    (toString [_] "Nothing")

    Monad
    (>>=    [_ f] nothing)
    (return [_ u] (just u))))

(defn m-do* [body]
  (match [body]
     [([val] :seq)] val
     [([fst & rst] :seq)] (match fst
                             [:let & vs] `(let [~@vs] ~(m-do* rst))
                             [k v] `(>>= ~v (fn [~k] ~(m-do* rst)))
                             v     `(>>= ~v (fn [_#] ~(m-do* rst)))))))

(defmacro m-do [& body]
  (m-do* body))

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




`~(inc 8)

(m-do 8)

(>>= (just 8) #(just [%1 %1]))
(>>= nothing #(just [%1 %1]))

(return [] 8)
(>>= [1 2] #(vector %1 %1))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
