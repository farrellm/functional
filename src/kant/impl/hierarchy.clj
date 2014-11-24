(ns kant.impl.hierarchy
  (:require [kant.impl.protocol]))

;;   functor
;;      |
;; applicative        category
;;      |    \----\   /
;;      |         arrow
;;      |           |
;;    monad    arrow-choice
;;        \---\     |
;;             arrow-apply

(derive ::applicative ::functor)
(derive ::monad ::applicative)

(derive ::arrow ::category)
(derive ::arrow-first ::arrow)
(derive ::arrow-choice ::arrow-first)
(derive ::arrow-apply ::arrow-choice)

(derive ::arrow ::applicative)
(derive ::arrow-apply ::monad)

(def arrow-apply  (cons [::arrow-apply  #'kant.impl.protocol/ArrowApply]  []))
(def arrow-choice (cons [::arrow-choice #'kant.impl.protocol/ArrowChoice] arrow-apply))
(def arrow-first  (cons [::arrow-first  #'kant.impl.protocol/ArrowFirst]  arrow-apply))
(def arrow        (cons [::arrow        #'kant.impl.protocol/Arrow]       []))
(def category     (cons [::category     #'kant.impl.protocol/Category]    arrow))

(def monad        (cons [::monad        #'kant.impl.protocol/Monad]       arrow-apply))
(def applicative  (cons [::applicative  #'kant.impl.protocol/Applicative] (concat monad arrow)))
(def functor      (cons [::functor      #'kant.impl.protocol/Functor]     applicative))

(def heirarchy (hash-map :arrow-apply arrow-apply
                         :arrow-choice arrow-choice
                         :arrow-first arrow-first
                         :arrow arrow
                         :category category
                         :monad monad
                         :applicative applicative
                         :functor functor))

(defn most-general [type inst]
  (some (fn [[t p]] (when (satisfies? (deref p) inst) t))
        (type heirarchy)))
