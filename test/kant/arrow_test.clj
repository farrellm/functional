(ns kant.arrow-test
  (:require [clojure.test :refer :all]
            [kant.impl.protocol :as p]
            [kant.impl.hierarchy :as h]
            [kant.monad :refer :all]
            [kant.category :refer :all]
            [kant.arrow :refer :all]
            [kant.arrow-choice :refer :all]
            [kant.arrow-apply :refer :all]
            [kant.arrow-loop :refer :all]
            [kant.arrow-proc :refer :all]
            [kant.monad.maybe :refer :all]
            [kant.monad.function :refer :all]
            [kant.monad.kleisli :refer :all]
            ))

(deftest arr-proc-function
  (testing "proc*"
    (is (= [8 9 7 63]
           ((-> (proc* '[x] [['y inc 'x]
                             ['z dec 'x]
                             [:return '(* y z)]])
                :arr) 8))))

  (testing "proc"
    (is (= [8 9]
           ((-> (proc [x]
                  [inc x])
                :arr) 8)))
    (is (= [8 9 7 63]
           ((-> (proc [x y z]
                  [identity (* y z)])
                :arr) 8 9 7)))
    (is (= [8 9 7 63]
           ((-> (proc [x]
                  [y inc x]
                  [z dec x]
                  [:return (* y z)])
                :arr) 8)))))

(deftest arr-proc-kleisli
  (let [m$ (kleisli just nil)
        m-inc (arr m$ inc)
        m-dec (arr m$ dec)]

    (testing "proc*"
      (is (= (just [8 9 7 63])
             ((-> (proc* '[x] [['y m-inc 'x]
                               ['z m-dec 'x]
                               [:return '(* y z)]])
                  :arr run-kleisli) 8))))

    (testing "proc"
      (is (= (just [8 8])
             ((-> (proc [x]
                    [(id m$) x])
                  :arr run-kleisli) 8)))
      (is (= (just [8 9 7 63])
             ((-> (proc [x y z]
                    [(id m$) (* y z)])
                  :arr run-kleisli) 8 9 7)))
      (is (= (just [8 9 7 63])
             ((-> (proc [x]
                    [y m-inc x]
                    [z m-dec x]
                    [:return (* y z)])
                  :arr run-kleisli) 8))))))
