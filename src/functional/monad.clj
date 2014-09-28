(ns functional.monad
  (:require [clojure.core.match :refer [match]]
            [functional.monad.protocol :refer :all]))

(declare <*>)
(declare <*)
(declare >>=)

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
  (if (satisfies? MonoidSum a) (-sum a as)
      (reduce -plus a as)))

;; Pure
(defn pure [m a]
  (-pure m a))

;; Functor
(defn fmap [f v]
  (if (satisfies? Functor v) (-fmap v f)
      (<*> (pure v f) v)))

;; Applicative
(defn <*>
  ([f] (fmap #(%) f))
  ([af av & avs] (cond
                  avs (apply <*> (<* af av) avs)
                  (satisfies? Applicative af) (-ap af av)
                  :else (>>= af (fn [f] (>>= av (fn [v] (pure af (f v)))))))))

(defn <*
  "partial application in an applicative"
  ([af] af)
  ([af a & r] (if r (apply <* (<* af a) r)
                  (<*> (fmap (fn [f] #(partial f %)) af) a))))

(defn m-sequence [x]
  (apply <*> (pure (first x) vector) x))

;; Category
(defn <<<
  ([a] a)
  ([a b & c] (if c (-comp a (apply <<< b c))
                 (-comp a b))))

(defn >>>
  ([a] a)
  ([a b & c] (if c (-comp (apply <<< b c) a)
                 (-comp b a))))

;; Arrow
(defn -swap [a b] [b a])
(defn -dup  [a]   [a a])

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

(defn applicative-fmap [v f]
  (<*> (pure v f) v))

(defn monad-<*> [af av]
  (m-do [f af]
        [v av]
        (pure af (f v))))

(defn lift [f]
  (fn [& m-args] (m-do [args (m-sequence m-args)]
                       [:return (apply f args)])))

(defn join [m]
  (>>= m identity))

(defn >=>
  ([f] f)
  ([f & fs] #(apply >>= (f %) fs)))
