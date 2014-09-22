(ns core.monad-test
  (:require [clojure.test :refer :all]
            [core.monad :refer :all]))

(deftest maybe
  (testing "basic"
    (is 8 (value (just 8))))
  (testing "functor"
    (is (just 9)(fmap inc (just 8))))
  (testing "applicative"
    (is (just 9) (<*> (just inc) (just 8))))
  (testing "monad"
    (is (just [8 8]) (>>= (just 8) #(just [%1 %1])))
    (is nothing (>>= nothing #(just [%1 %1]))))
  (testing "do"
    (is (just [1 5 3]) (m-do [x (just 1)]
                             (just 2)
                             [:let y 5, z 3]
                             (just [x y z]))))

  (testing "return"
    (is (just 8) (m-do [x (just 8)]
                       [:return x]))
    (is (just 8) (m-do (just 9)
                       [:return 1]
                       [:return 8])))
  (testing "multi-bind"
    (is (just [1 2]) (m-do [x (just 1)
                            y (just 2)]
                           [:return [x y]]))))

(deftest sequential
  (testing "functor"
    (is [2 3 4] (fmap inc [1 2 3]))
    (is [9] (fmap inc [8])))
  (testing "applicative"
    (is [8] (pure [] 8))
    (is [9] (<*> [inc] [8]))
    (is [2 3 4] (<*> (pure [] inc) [1 2 3]))

    (is [9] (<*> [#(identity 9)]))
    (is [9] (<*> (<* [identity] [9])))
    (is [9] (<*> (<* [inc] [8])))

    (is [0] (<*> [+]))
    (is [1] (<*> [+] [1]))
    (is [3] (<*> [+] [1] [2]))
    (is [3 4 4 5 5 6] (<*> [+] [1] [1 2] [1 2 3])))
  (testing "monad"
    (is [1 1 2 2] (>>= [1 2] #(vector %1 %1))))
  (testing "monoid"
    (is [] (mempty [1]))
    (is [1 2] (mappend [1] [2]))
    (is [1 2 3] (mconcat [1] [2] [3])))
  (testing "monad-plus"
    (is [1 2] (m-do [x [1 2 3 4]]
                    [:guard (< x 2.5)]
                    [:return x]))))

#_(deftest template
  (testing "functor")
  (testing "applicative")
  (testing "monad"))
