(ns kant.monad
  (:require [clojure.core.match :refer [match]]
            [kant.monad.protocol :refer :all])
  (:import [java.lang.UnsupportedOperationException]))

(declare <*>)
(declare <*)
(declare >>=)
(declare >>>)

(defn -swap [[a b]] [b a])
(defn -dup  [a]   [a a])
(defn -flip [f]   #(f %2 %1))

(defn $
  "partial application operator.  returns a function that expects to
  be partially applied 1 (more) time"
  [f]
  (fn [& args1]
    (fn [& args2]
      (apply (apply partial f args1) args2))))

;; Monoid
(defn zero [m]
  (-zero m))

(defn plus [a & as]
  (cond
   (satisfies? MonoidSum a) (-sum a as)
   (satisfies? Monoid a) (reduce -plus a as)
   :else (throw (UnsupportedOperationException. "Monoid/-plus"))))

;; Pure
(defn pure [m a]
  (cond
   (satisfies? Pure m) (-pure m a)
   (satisfies? Arrow m) (-arr m (fn [_] a))
   :else (throw (UnsupportedOperationException. "Pure/-pure"))))

;; Functor
(defn fmap [f v]
  (cond
   (satisfies? Functor v) (-fmap v f)
   (or (satisfies? Applicative v)
       (satisfies? Monad v)) (<*> (pure v f) v)
   :else (throw (UnsupportedOperationException. "Functor/-fmap"))))

;; Applicative
(defn <*>
  ([f] (fmap #(%) f))
  ([af av & avs] (cond
                  avs (apply <*> (<* af av) avs)
                  (satisfies? Applicative af) (-ap af av)
                  (satisfies? Monad af) (>>= af (fn [f] (>>= av (fn [v] (pure af (f v))))))
                  (satisfies? Arrow af) (>>> (-arr af -dup)  #(-first av %)
                                             (-arr af -swap) #(-first af %)
                                             (fn [[f v]] (f v)))
                  :else (throw (UnsupportedOperationException. "Applicative/-ap")))))

(defn <*
  "partial application in an applicative"
  ([af] af)
  ([af a & r] (if r (apply <* (<* af a) r)
                  (<*> (fmap (fn [f] #(partial f %)) af) a))))

(defn m-sequence [[a & as]]
  (apply <*> (pure a vector) a as))

;; Category
(defn <<<
  ([a] a)
  ([a b & c] (if c (-comp a (apply <<< b c))
                 (-comp a b))))

(defn >>>
  ([a] a)
  ([a b & c] (if c (-comp (apply >>> b c) a)
                 (-comp b a))))

;; Arrow
(defn arr [a f]
  (-arr a f))

(defn arr-first
  ([arr]   #(-first arr %))
  ([arr a] (-first arr a)))

(defn arr-second
  ([arr]   #(arr-second arr %))
  ([arr p] (->> (-swap p) (-first arr) (-swap))))

(defn ***
  ([f g]   #(*** f g %))
  ([f g p] (->> p (arr-first f) (arr-second g))))

(defn &&&
  ([f g]   #(&&& f g %))
  ([f g a] (*** f g [a a])))

;; Monad
(defn >>=
  ([m] m)
  ([m f & fs] (if fs (apply >>= (>>= m f) fs)
                  (-bind m f))))

(defn m-do*
  ([body] (m-do* body false))
  ([body type]
     (match [body]
            [([val] :seq)]       (match val
                                        [:return v] `(pure ~type ~v)
                                        v           v)
            [([fst & rst] :seq)] (match fst
                                        [:let & vs] `(let [~@vs] ~(m-do* rst type))
                                        [:return v] `(>>= (pure ~type ~v) (fn [_#] ~(m-do* rst type)))
                                        [:guard v]  `(>>= (if ~v (pure ~type nil) (zero ~type))
                                                          (fn [_#] ~(m-do* rst type)))
                                        [k v]       (if type
                                                      `(>>= ~v (fn [~k] ~(m-do* rst type)))
                                                      (let [t `t#]
                                                        `(let [~t ~v]
                                                           (>>= ~t (fn [~k] ~(m-do* rst t))))))
                                        [k v & rs]  (m-do* (concat [[k v]] [rs] rst) type)
                                        v           (if type
                                                      `(>>= ~v (fn [_#] ~(m-do* rst type)))
                                                      (let [t `t#]
                                                        `(let [~t ~v]
                                                           (>>= ~t (fn [_#] ~(m-do* rst t))))))))))

(defmacro m-do [& body]
  (m-do* body))

(defn lift [f]
  (fn [& m-args] (m-do [args (m-sequence m-args)]
                       [:return (apply f args)])))

(defn join [m]
  (>>= m identity))

(defn >=>
  ([f] f)
  ([f & fs] #(apply >>= (f %) fs)))
