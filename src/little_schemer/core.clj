(ns little-schemer.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def atom?
  (fn [a]
    (not (seq? a))))

(def null?
  (fn [a]
    (or
     (nil? a)
     (= () a))))

(def slist?
  (complement atom?))


(def charlie-lat?
  (fn
    [xs]
    (cond
     (atom? xs) false ;; it must be a list to be a lat
     (null? xs) true
     :else (and
            (atom? (first xs))
            (charlie-lat? (rest xs))))))

(def his-translated-lat?
  (fn [xs]
    (cond
     (null? xs) true
     (atom? xs) false ;; it must be a list to be a lat, still need this due to clojure vs scheme differences
     (atom? (first xs)) (his-translated-lat? (rest xs))
     :else false)))

(def member?
  (fn [a lat]
    (cond
     (null? lat) false
     :else (or (= (first lat) a)
               (member? a (rest lat))))))

(def rember
  (fn [a lat]
    (cond
     (null? lat) ()
     (= (first lat) a) (rest lat)
     :else (cons (first lat) (rember a (rest lat))))))

(def firsts
  (fn [xs]
    (cond
     (null? xs) xs
     :else (cons (first (first xs)) (firsts (rest xs))))))

(def insertR
  (fn [new old lat]
    (cond
     (null? lat) ()
     :else (cond
            (= (first lat) old) (cons old (cons new (rest lat)))
            :else (cons (first lat) (insertR new old (rest lat)))))))

(def insertL
  (fn [new old lat]
    (cond
     (null? lat) ()
     :else (cond
            (= (first lat) old) (cons new (cons old (rest lat))) ;; He points out, (cons old (rest lat)) is same here as (cons lat)
            :else (cons (first lat) (insertL new old (rest lat)))))))

(def subst
  (fn [new old lat]
    (cond
     (null? lat) ()
     :else (cond
            (= (first lat) old) (cons new (rest lat))
            :else (cons (first lat) (subst new old (rest lat)))))))

(def subst2
  (fn [new o1 o2 lat]
    (cond
     (null? lat) ()
     :else (cond
            (or (= (first lat) o1)
                (= (first lat) o2)) (cons new (rest lat))
                :else (cons (first lat) (subst2 new o1 o2 (rest lat)))))))

(def multirember
  (fn [a lat]
    (cond
     (null? lat) ()
     :else (cond
            (= (first lat) a) (multirember a (rest lat))
            :else (cons (first lat) (multirember a (rest lat)))))))

(def multiinsertR
  (fn [new old lat]
    (cond
     (null? lat) ()
     :else (cond
            (= (first lat) old) (cons old (cons new (multiinsertR new old (rest lat))))
            :else (cons (first lat) (multiinsertR new old (rest lat)))))))

(def multiinsertL
  (fn [new old lat]
    (cond
     (null? lat) ()
     :else (cond
            (= (first lat) old) (cons new (cons old (multiinsertL new old (rest lat))))
            :else (cons (first lat) (multiinsertL new old (rest lat)))))))

(def multisubst
  (fn [new old lat]
    (cond
     (null? lat) ()
     :else (cond
            (= (first lat) old) (cons new (multisubst new old (rest lat)))
            :else (cons (first lat) (multisubst new old (rest lat)))))))

(def add1
  (fn [x]
    (+ x 1)))

(def sub1
  (fn [x]
    (- x 1)))

(def plus
  (fn [a b]
    (cond
     (zero? a) b
     :else (plus (sub1 a) (add1 b))))) ;; he did this differently. he called add1 on (plus a (sub1 b)). He probably had a reason.

(def minus
  (fn [a b]
    (cond
     (zero? b) a
     :else (sub1 (minus a (sub1 b))))))

(def addtup
  (fn [xs]
    (cond
     (null? xs) 0
     :else (plus (first xs) (addtup (rest xs))))))

(def mult
  (fn [a b]
    (cond
     (zero? b) 0
     :else (plus a (mult a (sub1 b))))))

(def tup+
  (fn [tup1 tup2]
    (cond
     (and (null? tup1) (null? tup2)) () ;; I missed this, but this line is unnecessary. If they're both null, then tup1 is
;;                                          null and we will return tup 2 (which is null)
     (null? tup1) tup2
     (null? tup2) tup1
     :else (cons (plus (first tup1) (first tup2)) (tup+ (rest tup1) (rest tup2))))))

(def o>
  (fn [a b]
    (cond
     (zero? a) false
     (zero? b) true
     :else (o> (sub1 a) (sub1 b)))))

(def o<
  (fn [a b]
    (cond
     (zero? b) false
     (zero? a) true
     :else (o< (sub1 a) (sub1 b)))))

(def o=
  (fn [a b]
    (cond
     (and (zero? a) (zero? b)) true
     (or (zero? a) (zero? b)) false
     :else (o= (sub1 a) (sub1 b)))))

(def o=with-<-and->
  (fn [a b]
    (cond
     (o< a b) false
     (o> a b) false
     :else true)))

(def power
  (fn [a b]
    (cond
     (zero? b) 1
     :else (mult a (power a (sub1 b))))))

(def div
  (fn [a b]
    (cond
     (o< a b) 0
     :else (add1 (div (minus a b) b)))))

(def length
  (fn [xs]
    (cond
     (null? xs) 0
     :else (add1 (length (rest xs))))))

(def pick
  (fn [n xs]
    (cond
     (zero? n) nil  ;; he didn't even include this case. I interpreted his "no answer" bit to be a requirement to return nil
     (= 1 n) (first xs)
     :else (pick (sub1 n) (rest xs)))))

(def rempick
  (fn [n xs]
    (cond
     (zero? (sub1 n)) (rest xs)
     :else (cons (first xs) (rempick (sub1 n) (rest xs))))))

(def no-nums
  (fn [xs]
    (cond
     (null? xs) ()
     :else (cond
            (number? (first xs)) (no-nums (rest xs))
            :else (cons (first xs) (no-nums (rest xs)))))))

(def all-nums
  (fn [xs]
    (cond
     (null? xs) ()
     :else (cond
            (number? (first xs)) (cons (first xs) (all-nums (rest xs)))
            :else (all-nums (rest xs))))))

(def eqan?
  (fn [a b]
    (cond
     (and (number? a) (number? b)) (o= a b)
     :else (= a b))))

(def occur
  (fn [a xs]
    (cond
     (null? xs) 0
     :else (cond
            (eqan? a (first xs)) (add1 (occur a (rest xs)))
            :else (occur a (rest xs))))))

(def one?
  (fn [a]
    (eqan? a 1)))

(def rempick-with-one
  (fn [n xs]
    (cond
     (one? n) (rest xs)
     :else (cons (first xs) (rempick-with-one (sub1 n) (rest xs))))))
