(ns kant.hierarchy
  (:require [kant.monad.protocol]))

;;   functor
;;      |
;; applicative        category
;;      |    \----\   /
;;      |         arrow
;;      |           |
;;      |      arrow-choice
;;      |           |
;;    monad -- arrow-apply

(derive ::applicative ::functor)
(derive ::monad ::applicative)

(derive ::arrow ::category)
(derive ::arrow-choice ::arrow)
(derive ::arrow-apply ::arrow-choice)

(derive ::arrow ::applicative)

(def arrow-apply  (cons [::arrow-apply  #'kant.monad.protocol/ArrowApply]  []))
(def arrow-choice (cons [::arrow-choice #'kant.monad.protocol/ArrowChoice] arrow-apply))
(def arrow        (cons [::arrow        #'kant.monad.protocol/Arrow]       arrow-choice))
(def category     (cons [::category     #'kant.monad.protocol/Category]    arrow))

(def monad        (cons [::monad        #'kant.monad.protocol/Monad]       []))
(def applicative  (cons [::applicative  #'kant.monad.protocol/Applicative] (concat monad arrow)))
(def functor      (cons [::functor      #'kant.monad.protocol/Functor]     applicative))

(def heirarchy (hash-map :arrow-apply arrow-apply
                         :arrow-choice arrow-choice
                         :arrow arrow
                         :category category
                         :monad monad
                         :applicative applicative
                         :functor functor))

(defn most-general [type inst]
  (some (fn [[t p]] (when (satisfies? (deref p) inst) t))
        (type heirarchy)))
