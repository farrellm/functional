(ns kant.monad-test
  (:require [clojure.test :refer :all]
            [kant.monad :refer :all]
            [kant.monad.maybe :refer :all]
            [kant.monad.either :refer :all]
            [kant.monad.sequential :refer :all]
            [kant.monad.state :refer :all]
            [kant.monad.maybe-t :refer :all]
            ))

(deftest monad-functions
  (testing "m-sequence"
    (is (= (just [8 8])
           (m-sequence (repeat 2 (just 8)))))
    (is (= nothing
           (m-sequence [(just 8) nothing (just 8)]))))
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
    (is (= 8 (-left (left 8))))
    (is (= 8 (-right (right 8)))))
  (testing "functor"
    (is (= (left 8) (fmap inc (left 8))))
    (is (= (right 9) (fmap inc (right 8)))))
  (testing "applicative"
    (is (= (right 9) (<*> (right inc) (right 8))))
    (is (= (left 0) (<*> (left 0) (right 8))))
    (is (= (left 0) (<*> (right inc) (left 0))))
    (is (= (left 0) (<*> (left 0) (left 1)))))
  (testing "monad"
    (is (= (right [8 8]) (>>= (right 8) #(right [%1 %1]))))
    (is (= (right [9 9]) (>>= (right 8)
                              #(right (inc %))
                              #(right [%1 %1]))))
    (is (= nothing (>>= nothing #(right [%1 %1])))))
  (testing "do"
    (is (= (right [1 5 3]) (m-do [x (right 1)]
                                 (right 2)
                                 [:let y 5, z 3]
                                 (right [x y z]))))
    (is (= (left 0) (m-do [x (right 1)]
                                 (right 2)
                                 (left 0)
                                 [:let y 5, z 3]
                                 (right [x y z])))))

  (testing "return"
    (is (= (right 8) (m-do [x (right 8)]
                           [:return x])))
    (is (= (right 8) (m-do (right 9)
                           [:return 1]
                           [:return 8]))))
  (testing "multi-bind"
    (is (= (left 0) (m-do [x (right 1)
                           y (left 0)]
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

#_(deftest template
  (testing "functor")
  (testing "applicative")
  (testing "monad"))
