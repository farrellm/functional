(ns kant.hierarchy
  (:require [kant.monad.protocol]))

;;   functor
;;      |
;; applicative      category
;;      |     \     /
;;    monad    arrow
;;        \      +--------------+
;;          \    |              |
;;          arrow-apply    arrow-choice

(derive ::applicative ::functor)

(derive ::monad ::applicative)

(derive ::arrow ::category)
(derive ::arrow ::applicative)

(derive ::arrow-choice ::arrow)

(derive ::arrow-apply ::arrow)
(derive ::arrow-apply ::monad)

(def monad        (cons [::monad        #'kant.monad.protocol/Monad]       []))
(def arrow-apply  (cons [::arrow-apply  #'kant.monad.protocol/ArrowApply]  []))

(def arrow-choice (cons [::arrow-choice #'kant.monad.protocol/ArrowChoice] []))

(def arrow        (cons [::arrow        #'kant.monad.protocol/Arrow]       (concat arrow-apply
                                                                                   arrow-choice)))

(def applicative  (cons [::applicative  #'kant.monad.protocol/Applicative] (concat monad
                                                                                   arrow)))
(def functor      (cons [::functor      #'kant.monad.protocol/Functor]     applicative))

(def category     (cons [::category     #'kant.monad.protocol/Category]    arrow))

(def heirarchy (hash-map :monad monad
                         :arrow-choice arrow-choice
                         :arrow-apply arrow-apply
                         :arrow arrow
                         :category category
                         :applicative applicative
                         :functor functor))

(defn most-general [type inst]
  (some (fn [[t p]] (when (satisfies? (deref p) inst) t))
        (type heirarchy)))
