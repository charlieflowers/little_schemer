(ns little-schemer.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def atom?
  (fn [a]
    (not (seq? a))))

;; (def null?
;;   (fn [a]
;;     (println "checking if " a " is empty list or nil")
;;     (or
;;      (nil? a)
;;      (empty? a))))
;;
;; I was getting a stackoverflow as if null? had quit working, WAAY more than half way through the book. So I googled,
;;  and (seq coll) is the idiomatic was to check for empty list in clojure. So changing null? to do that.
;; It DID NOT HELP THE STACKOVERFLOW ONE FUCKING BIT.

(def null?
  (fn [xs]
    (seq xs)))

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

;; rember*: takes a list and an atom, and removes the atom from the list, but also, if the list contains lists, removes the atom
;;  from THOSE lists, and can go infinitely many levels deep.

(def rember*
  (fn [a xs]
    (cond
     (null? xs) ()
     :else (cond
            (slist? (first xs)) (cons (rember* a (first xs)) (rember* a (rest xs)))
            :else (cond
                   (eqan? (first xs) a) (rember* a (rest xs))
                   :else (cons (first xs) (rember* a (rest xs))))))))

(def insertR*
  (fn [new old xs]
    (cond
     (null? xs) () ;; note, at 1st i was surprised to see it doesn't work on vectors. But the def of atom? says that vectors are ATOMS!
     :else (cond
            (atom? (first xs)) (cond
                                (eqan? (first xs) old) (cons old (cons new (insertR* new old (rest xs))))
                                :else (cons (first xs) (insertR* new old (rest xs))))
            :else (cons (insertR* new old (first xs)) (insertR* new old (rest xs)))))))

;; Note: I see that he considers the question: (atom? (first xs)) to STILL BE ABOUT XS. And he likes to structure his fn's based
;;  on a series of questions about each arg. So he would prefer the following structure:

(def rember*-his-way
  (fn [a xs]
    (cond
     (null? xs) ()
     (atom? (first xs)) (cond
                         (eqan? (first xs) a) (rember*-his-way a (rest xs))
                         :else (cons (first xs) (rember*-his-way a (rest xs))))
     :else (cons (rember*-his-way a (first xs)) (rember*-his-way a (rest xs))))))

(def insertR*-his-way
  (fn [new old xs]
    (cond
     (null? xs) ()
     (atom? (first xs)) (cond
                         (eqan? (first xs) old) (cons old (cons new (insertR*-his-way new old (rest xs))))
                         :else (cons (first xs) (insertR*-his-way new old (rest xs))))
     :else (cons (insertR*-his-way new old (first xs)) (insertR*-his-way new old (rest xs))))))

(def occur*
  (fn [a xs]
    (cond
     (null? xs) 0
     (atom? (first xs)) (cond
                         (eqan? (first xs) a) (add1 (occur* a (rest xs)))
                         :else (occur* a (rest xs)))
     :else (plus (occur* a (first xs)) (occur* a (rest xs))))))

(def subst*
  (fn [new old xs]
    (cond
     (null? xs) ()
     (atom? (first xs)) (cond
                         (eqan? (first xs) old) (cons new (subst* new old (rest xs)))
                         :else (cons (first xs) (subst* new old (rest xs))))
     :else (cons (subst* new old (first xs)) (subst* new old (rest xs))))))

(def insertL*
  (fn [new old xs]
    (cond
     (null? xs) ()
     (atom? (first xs)) (cond
                         (eqan? (first xs) old) (cons new (cons old (insertL* new old (rest xs))))
                         :else (cons (first xs) (insertL* new old (rest xs))))
     :else (cons (insertL* new old (first xs)) (insertL* new old (rest xs))))))

(def member*             ; Sweet! Mine differs from his. At first, I thought mine was wrong, but it's RIGHT (his is too).
  (fn [a xs]             ; He uses an OR instead of COND right ...
    (cond
     (null? xs) false
     (atom? (first xs)) (cond
                         (eqan? (first xs) a) true      ; ... HERE! But it has exactly the same effect. Because the result is boolean.
                         :else (member* a (rest xs)))
     :else (or (member* a (first xs)) (member* a (rest xs))))))

;; Next up: leftmost. The leftmost atom in the first list, if you recurse all the way down. But if the first item eventually resolves
;;  to empty list, then "no answer"

;; His examples do not cover the case where the 1st item is an atom. But his verbal description sounds like it should return that atom.
;;  The interesting thing here is that, per his requirements, we don't actually recurse down the cdr! because the 1st item is either
;;  an atom (we're done) or a list (which better ultimately have an atom). We could care less about the cdr. Hmm....

(def leftmost
  (fn [xs]
    (cond
     (null? xs) nil
     (atom? (first xs)) (first xs)
     :else (leftmost (first xs)))))

;; Aha. He's saying that you are free to IGNORE the cases he tells you not to care about. As in "negative numbers DO NOT EXIST." In
;;  this case, he's saying i can feel totally free to ignore empty lists. So let's rewrite it that way.

(def leftmost-ignore-empty-lists
  (fn [xs]
    (cond
     (atom? (first xs)) (first xs)
     :else (leftmost (first xs)))))

;; The only problem with this is invalid input will cause a stack overflow. In Scheme, not, because of tail call opt (assuming we
;;  are careful to be in the tail position).

;;
;;
;; Next up, eqlist? And he says NINE questions are needed. I only see about 7. So I'm going to write it and see if i find 9.
;; I see, of course, that 3X3 is 9. But it seems to me some of those questions are unnecessary.
;;
;; If we follow his rule about "ask questions about 1 arg at a time," it will be verbose. Let's do it that way in case he wants it.

(def eqlist?
  (fn [xs ys]
    (cond
     (null? xs) (cond
                 (null? ys) true
                 :else false)
     (atom? (first xs)) (cond
                          (atom? (first ys)) (and (eqan? (first xs) (first ys)) (eqlist? (rest xs) (rest ys)))
                          :else false)
     :else (cond
            (null? ys) false
            (atom? (first ys)) false
            :else (and (eqlist? (first xs) (first ys)) (eqlist? (rest xs) (rest ys)))))))

;; To me, this could be shortened by allowing SOME of the questions to be asked about BOTH args at same time.
;; Like this:

(def eqlist-shorter?
  (fn [xs ys]
    (cond
     (and (null? xs) (null? ys)) true
     (and (atom? (first xs)) (atom? (first ys))) (and (eqan? (first xs) (first ys)) (eqlist-shorter? (rest xs) (rest ys)))
     (or (atom? (first xs)) (atom? (first ys))) false
     :else (and (eqlist-shorter? (first xs) (first ys)) (eqlist-shorter? (rest xs) (rest ys))))))


;; Both appear to be correct and handle several cases in repl.
;; Now, the 1st version asks 10 questions (surprise!) and the 2nd version asks 7 questions. Let's see what he has in mind...

;; Sweet! on page 92, he has almost exactly what I have in my shorter version.

;; So, when he first asks "how many questions", he really means, "What is the total number of all combinations." He also acknowledges
;;  we may be able to logically simplify it from there.

;; He says equal? (next up) needs 4 questions: is a an atom or list, and is b an atom or list. What he means is, 4 "cases"

;; NOTE: Here was my first attempt. It BLOWS UP, because I never explicitly deal with empty list! rest returns empty list, and that
;;  keeps going and going and going. Stackoverflow.

;; (def equal?
;;   (fn [a b]
;;     (cond
;;      (and (atom? a) (atom? b)) (eqan? a b)
;;      (or (atom? a) (atom? b)) false
;;      :else (and (equal? (first a) (first b)) (equal? (rest a) (rest b))))))

(def equal?
  (fn [a b]
    (cond
     (and (atom? a) (atom? b)) (eqan? a b)
     (or (atom? a) (atom? b)) false
     (null? a) (null? b) ;; Nice little shortcut here. If a is empty, then return whether or not b is empty.
     (null? b) false
     :else (and (equal? (first a) (first b)) (equal? (rest a) (rest b))))))

;; WOW! He winds up with EXACTLY my WRONG ANSWER! The one with the stack overflow! If that doesn't show I'm on the right track,
;;  I don't know what does! This must come down to some differences between Scheme and Clojure. Oh but wait ... he also delegates to
;;  eqlist?, which I don't. So the difference is there, because eqlist? does handle empty list.

;; Now he says rewrite eqlist? using equal?. What is the difference between the 2? eqlist? wants 2 lists, and it checks to see if
;;  they are equal. equal will take any 2 SEXPs, meaning atoms OR lists, and it will see if they are equal. So equal? does everything
;;  eqlist? does and more.

(def eqlist-with-equal?
  (fn [xs ys]
    ;; So we will follow his approach of acting as if edge cases we don't support don't exist. So I won't even check to
    ;;  see if they passed us lists. I will simple assume they did.
    (equal? xs ys)))

;; That was simple! In haskell, I'd just say eqlist-with-equal? = equal?. It'd just be some kind of alias.
;; This is totally different from what he said, and I don't see why.

;; Now things go a bit haywire. He says, "Here's rember". But what he presents is NOT rember. At best, it could be
;;  "rember*", but it seems slightly different from that as well. Then he simplifies it.

;; I think for now, I'll just focus on what he changes about the fn while simplifying it.
;;
;; OHH, WAIT, I GET IT! THE BOOK IS FINE. He is presenting a MODIFIED VERSION of rember that is capable of handling
;;  not just lats, but actual lists of sexp's. He presents it fully formed (did not ask me to write it), and he asks me if
;;  I can simplify it. Let's see.

(def rember-for-sexp-simplified
  (fn [sexp xs]
    (cond
     (null? xs) ()
     :else (cond
            (equal? (first xs) sexp) (rest xs)
            :else (cons (first xs) (rember-for-sexp-simplified sexp (rest xs)))))))

;; nailed it!!!

;; Then he asks, "is rember a * fn now?". I think the answer is yes, but he says no. wassup? I do see that we only recur down the cdr.
;;  But, we do recur there. And that's what fooled me: I tried some stuff where the sexp was buried deep in the cdr, and it worked.
;;  Then I tried some stuff where the sexp WAS the entire car, and it worked. But I did NOT try having the sexp buried deep in the
;;  car. So lemme try that (and I'll find that it does not work).
;; CONFIRMED, it did not work. So this is NOT a * fn.

;; He next asks "can it be further simplified?" I only see 1 thing: any time you have a cond inside the else of another cond, you
;;  at least MIGHT be able to promote some or its questions up to the top cond. Lemme try that.

(def rember-for-sexp-simplified-even-more
  (fn [sexp xs]
    (println "in badfn. sexp is " sexp " and xs is " xs)
    (null? xs) ()
    (println "made it past null check")
    (equal? (first xs) sexp) (rest xs)
    (println "made it past equal check")
    :else (cons (first xs) (rember-for-sexp-simplified-even-more sexp (rest xs)))))

;; That looks right to me, but it gets a stack overflow error. Let's figure out why. Blows on '(1) '(1)
;;
;; (rember-for-sexp-simplified-even-more '(1) '(1))
;; sexp = '(1) and xs = '(1)
;; null? no
;; (equal 1 '(1))?
;; Calling? equal? a=1, b='(1)
;; One is an atom, the other is not. So equal returns false.
;; (cons 1 (rember-for-sexp-simplified-even-more '(1) ()))
;; (cons 1 ())
;; '(1)
;; That is the right result. That's what my hand eval says I should get. But instead I get stackoverflow!
;;
;; Let's do it again.
;;
;; (rember-for-sexp-simplified-even-more '(1) '(1))
;; sexp = '(1)
;; xs = '(1)
;; (null? xs) -- no
;; (equal? (first xs) sexp)
;; (equal? 1 '(1))
;; a = 1 and b = '(1)
;; both atoms? no
;; either one an atom? Yes. So return false.
;;
;; (cons (first xs) (rember-for-sexp-simplified-even-more sexp (rest xs)))
;; (cons 1 (rember-for-sexp-simplified-even-more '(1) ()))
;; sexp = '(1) and xs = ()
;; null? xs -- yes. Return ().
;;
;; (cons 1 ())
;; '(1)
;;
;; Hey! It's getting the stackoverflow in atom?!! Wish I'd noticed that sooner. Lets see wassup.
;; a = 1 and b = '(1)
;;
     ;; (and (atom? a) (atom? b)) (eqan? a b)
     ;; (or (atom? a) (atom? b)) false

;; (atom? 1) -- true
;; (atom? '(1)) -- false. Working just fine
;;
;; So it's not that.
;; Let's do some println debugging!
;;
;; So atom? is fine. The stackoverflow can hit atr random places, just when you run out of stack space.

;; At this point, there's nothing to do but walk away. I have no idea what the fuck is going on. It checks to see if () is an empty
;;  list, and it gets false! And for DAYS now, that check has been working. I changed it to use (seq xs) because that's idiomatic
;;  clojure, but it STILL fails.
;;
;; Maybe I'll see the problem in the morning.
