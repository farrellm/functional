(ns kant.hierarchy
  (:require [kant.monad.protocol]))

;;   functor
;;      |
;; applicative   category
;;         \       /
;;           arrow
;;             |
;;           monad

(derive ::applicative ::functor)
(derive ::arrow ::applicative)
(derive ::arrow ::category)
(derive ::monad ::arrow)

(def monad       (cons [::monad       #'kant.monad.protocol/Monad]       []))
(def arrow       (cons [::arrow       #'kant.monad.protocol/Arrow]       monad))
(def category    (cons [::category    #'kant.monad.protocol/Category]    arrow))
(def applicative (cons [::applicative #'kant.monad.protocol/Applicative] arrow))
(def functor     (cons [::functor     #'kant.monad.protocol/Functor]     applicative))

(def heirarchy (hash-map :monad monad
                         :arrow arrow
                         :category category
                         :applicative applicative
                         :functor functor))

(defn most-general [type inst]
  (some (fn [[t p]] (when (satisfies? (deref p) inst) t))
        (type heirarchy)))
