(ns kant.monad-test
  (:require [clojure.test :refer :all]
            [kant.impl.protocol :as p]
            [kant.impl.hierarchy :as h]
            [kant.functor :refer :all]
            [kant.applicative :refer :all]
            [kant.monad :refer :all]
            [kant.category :refer :all]
            [kant.arrow :refer :all]
            [kant.arrow-choice :refer :all]
            [kant.arrow-apply :refer :all]
            [kant.monoid :refer :all]
            [kant.monad.maybe :refer :all]
            [kant.monad.either :as e]
            [kant.monad.sequential :refer :all]
            [kant.monad.state :refer :all]
            [kant.monad.maybe-t :refer :all]
            [kant.monad.function :refer :all]
            [kant.monad.kleisli :refer :all]
            ))

(deftest monad-functions
  (testing "sequence"
    (is (= (just [8 8])
           (sequence-a (repeat 2 (just 8)))))
    (is (= nothing
           (sequence-a [(just 8) nothing (just 8)]))))
  (testing "lift"
    (is (= (just 24)
           ((lift +) (just 8) (just 8) (just 8)))))
  (testing "join"
    (is (= [8] (join [[8]])))))

(deftest maybe-
  (testing "basic"
    (is (= 8 (value (just 8)))))
  (testing "functor"
    (is (= (just 9) (fmap inc (just 8)))))
  (testing "applicative"
    (is (= (just 9) (<*> (just inc) (just 8)))))
  (testing "monad"
    (is (= (just [8 8]) (>>= (just 8) #(just [%1 %1]))))
    (is (= (just [9 9]) (>>= (just 8)
                             #(just (inc %))
                             #(just [%1 %1]))))
    (is (= nothing (>>= nothing #(just [%1 %1])))))
  (testing "do"
    (is (= (just [1 5 3]) (m-do [x (just 1)]
                                (just 2)
                                [:let y 5, z 3]
                                (just [x y z])))))

  (testing "return"
    (is (= (just 8) (m-do [x (just 8)]
                          [:return x])))
    (is (= (just 8) (m-do (just 9)
                          [:return 1]
                          [:return 8]))))
  (testing "multi-bind"
    (is (= (just [1 2]) (m-do [x (just 1)
                               y (just 2)]
                              [:return [x y]])))))

(deftest either-
  (testing "basic"
    (is (= 8 (e/-left (e/left 8))))
    (is (= 8 (e/-right (e/right 8)))))
  (testing "functor"
    (is (= (e/left 8) (fmap inc (e/left 8))))
    (is (= (e/right 9) (fmap inc (e/right 8)))))
  (testing "applicative"
    (is (= (e/right 9) (<*> (e/right inc) (e/right 8))))
    (is (= (e/left 0) (<*> (e/left 0) (e/right 8))))
    (is (= (e/left 0) (<*> (e/right inc) (e/left 0))))
    (is (= (e/left 0) (<*> (e/left 0) (e/left 1)))))
  (testing "monad"
    (is (= (e/right [8 8]) (>>= (e/right 8) #(e/right [%1 %1]))))
    (is (= (e/right [9 9]) (>>= (e/right 8)
                                #(e/right (inc %))
                                #(e/right [%1 %1]))))
    (is (= nothing (>>= nothing #(e/right [%1 %1])))))
  (testing "do"
    (is (= (e/right [1 5 3]) (m-do [x (e/right 1)]
                                   (e/right 2)
                                   [:let y 5, z 3]
                                   (e/right [x y z]))))
    (is (= (e/left 0) (m-do [x (e/right 1)]
                            (e/right 2)
                            (e/left 0)
                            [:let y 5, z 3]
                            (e/right [x y z])))))

  (testing "return"
    (is (= (e/right 8) (m-do [x (e/right 8)]
                             [:return x])))
    (is (= (e/right 8) (m-do (e/right 9)
                             [:return 1]
                             [:return 8]))))
  (testing "multi-bind"
    (is (= (e/left 0) (m-do [x (e/right 1)
                             y (e/left 0)]
                            [:return [x y]])))))

(deftest sequential
  (testing "functor"
    (is (= [2 3 4] (fmap inc [1 2 3])))
    (is (= [9] (fmap inc [8]))))
  (testing "applicative"
    (is (= [8] (pure [] 8)))
    (is (= [9] (<*> [inc] [8])))
    (is (= [2 3 4] (<*> (pure [] inc) [1 2 3])))

    (is (= [9] (<*> [#(identity 9)])))
    (is (= [9] (<*> (<* [identity] [9]))))
    (is (= [9] (<*> (<* [inc] [8]))))

    (is (= [0] (<*> [+])))
    (is (= [1] (<*> [+] [1])))
    (is (= [3] (<*> [+] [1] [2])))
    (is (= [3 4 5 4 5 6] (<*> [+] [1] [1 2] [1 2 3]))))
  (testing "monad"
    (is (= [1 1 2 2] (>>= [1 2] #(vector %1 %1)))))
  (testing "monoid"
    (is (= [] (zero [1])))
    (is (= [1 2] (plus [1] [2])))
    (is (= [1 2 3] (plus [1] [2] [3]))))
  (testing "monad-plus"
    (is (= [1 2] (m-do [x [1 2 3 4]]
                       [:guard (< x 2.5)]
                       [:return x])))))

(deftest state-
  (testing "state"
    (is (= [[9 10] 1])
        ((run-state (m-do (put 8)
                          (modify inc)
                          [x (gets)
                           y (gets inc)]
                          (put 1)
                          [:return [x y]])) []))))

(deftest maybe-t-
  (testing "mt"
    (is (= (maybe-t [(just 2) nothing (just 3)])
           (>>= (maybe-t [(just 1) nothing (just 2)])
                #(maybe-t [(just (inc %))]))))))

(deftest function
  (testing "arrow"
    (is (= [11 200] ((xxx inc #(+ % %)) [10 100])))
    (is (= [11 20]  ((&&& inc #(+ % %)) 10)))

    (is (= 1 ((arr #() inc) 0)))
    (is (= [1 2] ((arr-first inc) [0 2])))
    (is (= [0 3] ((arr-second inc) [0 2])))

    (is (=  1 ((<<< dec #(* 2 %) inc) 0)))
    (is (= -1 ((>>> dec #(* 2 %) inc) 0))))

  (testing "applicative"
    (is (= 9 ((<*> (constantly inc) (constantly 8)) nil)))
    (is (= 9 ((<*> (pure #() inc) (pure #() 8)) nil))))

  (testing "monad"
    (is (= [8 8] ((>>= identity #(partial conj [%])) 8)))
    (is (= 19 ((m-do [a #(* 2 %)]
                     [b #(+ 10 %)]
                     [:return (+ a b)])
               3)))))

(def k (kleisli just #()))
(deftest kleisli-
  (testing "arrow"
    (is (= (just [11 200]) ((run-kleisli (xxx (arr k inc) (arr k #(+ % %)))) [10 100])))
    (is (= (just [11 20])  ((run-kleisli (&&& (arr k inc) (arr k #(+ % %)))) 10)))

    (is (= (just 1) ((run-kleisli (arr k inc)) 0)))
    (is (= (just [1 2]) ((run-kleisli (arr-first (arr k inc))) [0 2])))
    (is (= (just [0 3]) ((run-kleisli (arr-second (arr k inc))) [0 2])))

    (is (= (just 1)  ((run-kleisli (<<< (arr k dec) (arr k #(* 2 %)) (arr k inc))) 0)))
    (is (= (just -1) ((run-kleisli (>>> (arr k dec) (arr k #(* 2 %)) (arr k inc))) 0)))
    )

  (testing "applicative"
    (is (= (just 9) ((run-kleisli (<*> (arr k (constantly inc)) (arr k (constantly 8)))) nil)))
    (is (= (just 9) ((run-kleisli (<*> (arr k (pure #() inc)) (arr k (pure #() 8)))) nil))))

  (testing "monad"
    (is (= (just [8 8]) ((run-kleisli (>>= (arr k identity)
                                           #(arr k (partial conj [%])))) 8)))
    (is (= (just 19) ((run-kleisli (m-do [a (arr k #(* 2 %))]
                                         [b (arr k #(+ 10 %))]
                                         [:return (+ a b)]))
                      3))))
  )

#_(deftest template
  (testing "functor")
  (testing "applicative")
  (testing "monad"))
