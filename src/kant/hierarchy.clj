(ns kant.hierarchy
  (:require [kant.monad.protocol]))

;;   functor
;;      |
;; applicative   category
;;         \       /
;;           arrow
;;             |
;;        arrow-choice
;;             |
;;           monad

(derive ::applicative ::functor)
(derive ::arrow ::applicative)
(derive ::arrow ::category)
(derive ::arrow-choice ::arrow)
(derive ::monad ::arrow-choice)

(def monad        (cons [::monad        #'kant.monad.protocol/Monad]       []))
(def arrow-choice (cons [::arrow-choice #'kant.monad.protocol/ArrowChoice] monad))
(def arrow        (cons [::arrow        #'kant.monad.protocol/Arrow]       arrow-choice))
(def category     (cons [::category     #'kant.monad.protocol/Category]    arrow))
(def applicative  (cons [::applicative  #'kant.monad.protocol/Applicative] arrow))
(def functor      (cons [::functor      #'kant.monad.protocol/Functor]     applicative))

(def heirarchy (hash-map :monad monad
                         :arrow-choice arrow-choice
                         :arrow arrow
                         :category category
                         :applicative applicative
                         :functor functor))

(defn most-general [type inst]
  (some (fn [[t p]] (when (satisfies? (deref p) inst) t))
        (type heirarchy)))
