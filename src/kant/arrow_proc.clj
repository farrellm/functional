(ns kant.arrow-proc
  (:require [kant.category :refer :all]
            [kant.arrow :refer :all]
            [kant.arrow-choice :refer :all]
            [kant.arrow-apply :refer :all]
            [kant.monad.function]
            [clojure.pprint :refer :all]
            [clojure.core.match :refer [match]]))

(comment
  [x]   [f y]                     (>>> (arr _ (fn [x] y)) f)
  [x y] [(delay false) (and x y)] (>>> (arr _ (fn [[x y] (and x y)])) (delay false))
  [f x] [:app f x]                (>>> (arr _ (fn [[f x]] [f x])) >>> (app _))
  [x]   [:if [p x] [f x] [g x]]   (>>> (arr _ (fn [x] (if (p x) (left x) (right x))) (||| f g)))

  [[y f (+ x 1)]
   [  g (* 2 y)]
   [:let [z (+ x y)]]
   [t h (* x z)]
   [:return (+ t z)]]
  )

(defn -symbols [res exp]
  (filter (into #{} (-> res :vals keys)) (flatten [exp])))

(defn -proc-line [res line]
  (match line
    [:return exp] (-proc-line res [(arr (:type res) identity) exp])
    [a exp] (if (:type res)
              (let [ss (-symbols res exp)
                    arr-exp (arr a (fn [v] (apply (eval `(fn [ ~@ss ] ~exp)) v)))]
                (-> res
                    (update-in [:arr] >>>
                               (arr a (fn [v] [(map #(v (% (:vals res))) (-symbols res exp)) v]))
                               (arr-first arr-exp)
                               (arr-first a)
                               (arr a (fn [[x v]] (assoc v (:index res) x))))
                    (update-in [:index] inc)))
              (-proc-line (assoc res
                            :type a
                            :arr  (arr a (:args res))) line))
    [pat a exp] (-> (-proc-line res [a exp])
                    (update-in [:vals] assoc pat (:index res)))))

(defn proc* [args body]
  (reduce -proc-line {:vals (zipmap args (range))
                      :arr nil
                      :args (fn [& as]
                              (assert (= (count as) (count args)))
                              (into [] as))
                      :index (count args)
                      :type nil} body))



(defn -quote-body [body]
  (mapv (fn [line] (match line
                     [:return exp] [:return `(quote ~exp)]
                     [a exp] [a `(quote ~exp)]
                     [pat a exp] [`(quote ~pat) a `(quote ~exp)]))
        body))

(defmacro proc [args & body]
  `(proc* '~args ~(-quote-body body)))
