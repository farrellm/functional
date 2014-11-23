(ns kant.monad.kleisli
  (:require [kant.impl.protocol :as p]
            [kant.monad :refer :all]
            [kant.monad.either :as e]
            [clojure.core.match :refer [match]]))

(defprotocol Kleisli
  (run-kleisli [_]))

(defn kleisli [m f]
  (reify
    Kleisli
    (run-kleisli [_] f)

    p/Category
    (-id [_] (kleisli m #(m %)))
    (-comp [_ b] (kleisli m (>=> (run-kleisli b) f)))

    p/Arrow
    (-arr [_ f] (kleisli m #(m (f %))))
    p/ArrowFirst
    (-first [_] (kleisli m (fn [[a1 b]]
                             (m-do [a2 (f a1)]
                                   [:return [a2 b]]))))
    ;; p/ArrowSecond
    ;; (-second [_] (kleisli m (fn [[a b1]]
    ;;                           (m-do [b2 (f b1)]
    ;;                                 [:return [a b2]]))))

    p/ArrowChoice
    (-left [_] (kleisli m  #(match [%]
                              [{:left v}]  (m-do [u (f v)]
                                                 (m (e/left u)))
                              [{:right v}] (m (e/right v)))))
    ;; p/ArrowChoiceRight
    ;; (-right [_] (kleisli m  #(match [%]
    ;;                           [{:left v}]  (m (e/left v))
    ;;                           [{:right v}] (m-do [u (f v)]
    ;;                                              (m (e/right u))))))

    p/ArrowApply
    (-app [_] (kleisli m (fn [[a b]] ((run-kleisli  a) b))))
    ))
