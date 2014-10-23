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
