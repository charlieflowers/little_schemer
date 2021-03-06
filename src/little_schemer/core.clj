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
    (empty? xs)))

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

;; (def rember-for-sexp-simplified-even-more
;;   (fn [sexp xs]
;;     (println "in badfn. sexp is " sexp " and xs is " xs)
;;     (null? xs) ()
;;     (println "made it past null check")
;;     (equal? (first xs) sexp) (rest xs)
;;     (println "made it past equal check")
;;     :else (cons (first xs) (rember-for-sexp-simplified-even-more sexp (rest xs)))))

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

;; Here we are the next day (10/25/2014). I'm going to verify the UNSIMPLIFIED fn, and then re-write the simplified one, and see
;;  where we are. ALSO, I know I did the OPPOSITE of the proper change when I did (seq coll). It should be empty? coll.

(def rember-for-sexp-mother-of-god-help
  (fn [sexp xs]
    (cond
     (null? xs) ()
     (equal? (first xs) sexp) (rest xs)
     :else (cons (first xs) (rember-for-sexp-mother-of-god-help sexp (rest xs))))))

;; Well holy shit, this works right out of the gate. Thankfully. But WHAT WAS THE PROBLEM?!?!
;;  for comparison, here's the broken one:

;; (def rember-for-sexp-simplified-even-more
;;   (fn [sexp xs]
;;     (println "in badfn. sexp is " sexp " and xs is " xs)
;;     (null? xs) ()
;;     (println "made it past null check")
;;     (equal? (first xs) sexp) (rest xs)
;;     (println "made it past equal check")
;;     :else (cons (first xs) (rember-for-sexp-simplified-even-more sexp (rest xs)))))

;;  HAHHA HAHAHHA HHHAAAAAAAAAA HAAAAAAAAAAAAAAAAAAAAAAAAAAAAA! OMG!
;;  You have to laugh or you would FUCKING CRY!
;;  I left off the call to COND!!! I simply listed a SERIES OF STATEMENTS! Mother of God indeed!
;; So let's figure out what it was doing. First, (null? xs) was called. It returned false, which was IGNORED. Then,
;;  the expression () was executed. Then, the println. Then, we checked if the first == sexp. Then, we calculated (rest xs).
;;  Then, we hit a :else. Was that merely executed???? Then, we recurred on the rest. Yes, it IS changing the variable towards the
;;  base case each time, but it NEVER stops recurring. It will recur on empty list forever (after it finishes processing the actual
;;  contents of the list).

;; Well ... that was special. Moving on....

;; Nicely, my simplified version matches his exactly.

;; Now he says simplify insertL*

(def insertL*-simplified
  (fn [new old xs]
    (cond
     (null? xs) ()
     (atom? (first xs)) (cond
                         (eqan? (first xs) old) (cons new (cons old (insertL*-simplified new old (rest xs))))
                         :else (cons (first xs) (insertL*-simplified new old (rest xs))))
     :else (cons (insertL*-simplified new old (first xs)) (insertL* new old (rest xs))))))

;; This can't be simplified. The inner cond is on a meaningful conditional branch, not on :else

;; He asks, "Can all fns that use eq? and = be changed to use eqan?" Gotta remember, in his world, = compares numbers, and
;;  eq compares sexps. I don't think he has a primitive that can check equality of lists. So I think they can (except of course
;;  for eqan? itself, which needs to use these 2.) But let's look deeper, into o=. Ah, there is one thing. o= pretends that negative
;;  numbers don't exist. So it cannot handle negative numbers. Let's see his answer.

;; Ok, he agrees with me. He continues to pretend negs don't exist, and he says other than eqan, we can rewrite all fns to use eqan.

;; Keep in mind, he says an expression is an ATOM (or some combo). So 'fuck is an expression.
;; And parentheses are "not really there". We just have to use them so we can pass around sexps.

;; numbered? asks if the expression is nothing but numbers and + X ^

(def valid-operators #{'+ '* '% }) ;; I'm using % for power because clojure takes over ^ and most other symbols.

(def charlie-numbered?
  (fn [sexp]
    (cond
     (atom? sexp) (or (number? sexp) (valid-operators sexp))
     (null? sexp) true
     :else (and (charlie-numbered? (first sexp)) (charlie-numbered? (rest sexp))))))

;; After writing the above and looking at his answer, I see he has more requirements that he did not mention. He is actually
;;  looking at the second member of the sexp and expecting it to be an operator. So everything is gets is of the form
;;  (number op number). So I will read on before trying to implement things till his intent is clear.

;; I see. He expects to see (expr1 op expr2) or (numeric-atom). Each expression is a sexp, so parsing is handled for us by the reader.
;; So he is going further than my fn. He not only requires all numbers and operators. He also requires the operators to be in
;;  the right place (infix).

;; (def numbered?
;;   (fn [sexp]
;;     (cond
;;      (atom? sexp) (number? sexp)
;;      (null? sexp) true ;; He doesn't have this line, but I don't see how recursion stops without it.
;;      (null? (rest sexp)) (numbered? (first sexp))
;;      (valid-operators (first (rest sexp))) (and (numbered? (first sexp)) (numbered? (rest (rest sexp))))
;;      :else false))

;; I think I'm starting to intuit what he wants. A straight atom is ok: (numbered? 42) is true.
;; But if the sexp is a list, then it MUST have an operator in the 2nd slot. So (numbered? '(1)) is UNDEFINED. It is "no answer".
;;  It is outside the requirements, so we shall pretend it does not exist.
;;
;; So the cases are:
;; atom?
;; Is second item a valid operator?
;; else false.

;; (def numbered?
;;   (fn [sexp]
;;     (cond
;;      (atom? sexp) (number? sexp)
;;      (valid-operators (first (rest sexp))) (and (numbered? (first sexp)) (numbered? (rest (rest sexp))))
;;      :else false)))

;; That was a reasonable thought process, but flawed. consider (numbered? '(1 + 2)). We expect true. However, ultimately, it will
;; come down to (numbered? (rest (rest sexp))), which would be (numbered '(2)). That would need to be found true, but our def makes
;;  it false. The 2 (or whatever is on the right hand side) will be recursed on as a list.
;; So, if the second item in the list is not an operator, then we must have only 1 list, and it must be numbered?.

(def numbered?
  (fn [sexp]
    (cond
     (atom? sexp) (number? sexp)
     (valid-operators (first (rest sexp))) (and (numbered? (first sexp)) (numbered? (rest (rest sexp))))
     (null? (rest sexp)) (numbered? (first sexp)) ;; this is the new line
     :else false)))

;; He then wants me to reflect: since sexp is a list, why aren't there only 2 questions -- empty and else? The answer is, the
;;  sexp param is not really a list. It is an "arithmetic expression" in list form. And so as always, we handle all the "cases" of
;;  arithemtic expression. There are only 2: it is either a number or it is 2 expressions separated by an operator. The only weird
;;  thing is that I found a 3rd case: A list containing only 1 item. I suppose we COULD get rid of that case by taking the FIRST
;;  of the final list. It would ignore any garbage on the end of the list, but he is not one for error checking anyway.
;;  So here's that version:

(def numbered-2-cases-only
  (fn [sexp]
    (cond
     (atom? sexp) (number? sexp)
     (valid-operators (first (rest sexp))) (and (numbered-2-cases-only (first sexp)) (numbered-2-cases-only (first (rest (rest sexp)))))
     ;; The key change is here: .............................................................................^^^^^
     :else false)))

;; Now, the 2 in '(1 + 2) will be an atom, not a list. But no, that won't work! What about '(1 + (2 + 3))? It works, but I think I
;;  would incorrectly say true for (1 + (2 g 3)).
;;
;; Seems like a catch-22. But I know I am missing something that will actually make it SIMPLER. Need to find it.
;; But wait!! ;; strangely, '(1 + (2 g 3)) DOES work. I correctly say false. Why?
;; Ah, I see! At one point, my fn splits '(1 + (2 g 3)) up, and it checks 1, and then it
;; checks (2 g 3). It gets to the (2 g 3) part by calling (first (rest (rest sexp))). The inner rest returns '( + (2 g 3)). I knew that.
;;  But I thought the next rest would return (2 g 3), and then first would return 2 (ignoring the illegal g). But that's wrong!
;; Instead, that second rest (in execution order) returns ((2 g 3)). A LIST containing the list (2 g 3). Remember, rest always returns
;;  a list. This time, it's a list containing a list. So then, "first" returns the whole list (2 g 3), which is exactly what we want.
;;
;; All this to say, the fn i wrote was correct and I should've never doubted myself! (ahem)
;;
;; So this is the kind of confusion over destructuring a deeply nested list that experience can help me get past.

;; He asks, "since aexp was already understood to be an arithmetic expression, can we write the fn simpler?" The only thing I think
;;  he can be getting at is that we don't have to check that the operators are valid. We can assume we were given a valid aexp, and
;;  therefore, just act on it. That would be like this:

(def numbered-simpler?
  (fn [aexp]
    (cond
     (atom? aexp) (number? aexp)
     :else (and (numbered-simpler? (first aexp)) (numbered-simpler? (first (rest (rest aexp))))))))

;; This MIGHT be ok; his requirements are unclear. It allows anything through as an operator. But if you put non-numbers in the
;;  non-operator slots, it will reject you. (Judgy, I know)

;; And bingo, that's exactly what he had in mind.

;; He's trying to make the point: Go ahead and write the fn with the painful, awful duplication (he had to repeat the big
;;  "and" clause from my version FOUR TIMES). Get it right and know you got it right. THEN and ONLY THEN, simplify it.

;; Next up: value!

(def value
  (fn [nexp]
    (cond
     (atom? nexp) nexp ;; No requirement for error checking that its not a number
     (eqan? '+ (first (rest nexp))) (plus (value (first nexp)) (value (first (rest (rest nexp)))))
     (eqan? '* (first (rest nexp))) (mult (value (first nexp)) (value (first (rest (rest nexp)))))
     (eqan? '% (first (rest nexp))) (power (value (first nexp)) (value (first (rest (rest nexp))))))))

;; In his spirit of ignoring unsupported cases, I'm not even putting an else onto that cond.
;; He also ignored possibility of erroneous input. He replaced my 3rd question with else, which makes sense, because he says
;;  else is always the last question (maybe in scheme it has to be?)

(def value-prefix
  (fn [nexp]
    (cond
     (atom? nexp) nexp
     (eqan? '+ (first nexp)) (plus (value-prefix (first (rest nexp))) (value-prefix (first (rest (rest nexp)))))
     (eqan? '* (first nexp)) (mult (value-prefix (first (rest nexp))) (value-prefix (first (rest (rest nexp)))))
     (eqan? '% (first nexp)) (power (value-prefix (first (rest nexp))) (value-prefix (first (rest (rest nexp))))))))

;; So in the book, he expected you to fall into a trap here, but I didn't. I speculate that the error he expected has something to
;;  do with thinking of lists instead of subexpressions. I* wish I knew exaclty what he was getting at so I make sure I see the
;;  principle. I guess if we just blindly went down the habitual path of lists, we'd screw up. But its obvious if you want to add
;;  the right things, you have to get to them properly. I guess the lesson is, "recurse on the subparts, so make sure you get the
;;  subparts correctly."

(def first-sub-exp
  (fn [aexp]
    (first (rest aexp))))

(def second-sub-exp
  (fn [aexp]
    (first (rest (rest aexp)))))

(def operator
  (fn [aexp]
    (first aexp)))

(def value-prefix-with-fns
  (fn [nexp]
    (cond
     (atom? nexp) nexp
     (eqan? (operator nexp) '+) (plus (value-prefix-with-fns (first-sub-exp nexp)) (value-prefix-with-fns (second-sub-exp nexp)))
     (eqan? (operator nexp) '*) (mult (value-prefix-with-fns (first-sub-exp nexp)) (value-prefix-with-fns (second-sub-exp nexp)))
     (eqan? (operator nexp) '%) (power (value-prefix-with-fns (first-sub-exp nexp)) (value-prefix-with-fns (second-sub-exp nexp))))))

;; Nice, he shows how to change from infix to prefix by parameterizing first-sub-exp and second-sub-exp and operator.

;; Primitives needed for numbers: number?, zero?, add1 and sub1

(def sero?
  (fn [x]
    (cond
     (atom? x) false       ;; He doesn't even put this line. As always, he ignores the possibility of bad input.
     :else (null? x))))

(def edd1
  (fn [xs]
    (cons () xs)))

(def zub1
  (fn [xs]
    (rest xs)))

(def zplus
  (fn [a b]
    (cond
     (sero? b) a
     :else (edd1 (zplus a (zub1 b))))))

;; Ahh, cool! He's pointing out that the only thing that changed between zplus and plus is the PRIMITIVIES!

;; Cliffhanger here at the end of this chapter. lat would return false for ( (()) ( () () ) ( () () () )), even though with our
;;  new number representation, that's (1 2 3) and so we'd like lat to return true. The only thing he says about it is YOU MUST
;;  BEWARE OF SHADOWS ... (and then the chapter ends).

;; (def zet?
;;   (fn [xs]
;;     (cond
;;      (null? xs) true
;;      :else (and (eqan? (occur (first xs) (rest xs)) 0) (zet? (rest xs))))))

;; His is very similar, except he used member? That is a better choice, so rewriting:

(def zet?
  (fn [xs]
    (cond
     (null? xs) true
     :else (and (not (member? (first xs) (rest xs))) (zet? (rest xs))))))

;; Then he says simplify zet. I notice, anytime you have an and inside the else of a cond, you can probably simplify it.

(def zet-simplified?
  (fn [xs]
    (cond
     (null? xs) true
     (member? (first xs) (rest xs)) false
     :else (zet-simplified? (rest xs)))))

(def makeset
  (fn [xs]
    (cond
     (null? xs) ()
     :else (cond
            (member? (first xs) (rest xs)) (makeset (rest xs))
            :else (cons (first xs) (makeset (rest xs)))))))

(def makeset-with-multirember
  (fn [xs]
    (cond
     (null? xs) ()
     :else (cons (first xs) (makeset-with-multirember (multirember (first xs) (rest xs)))))))

(def subset?
  (fn [set1 set2]
    (cond
     (null? set1) true
     :else (and (member? (first set1) set2) (subset? (rest set1) set2)))))

;; Now, he says shorter one, which means move that and condition up.

(def subset-shorter?
  (fn [set1 set2]
    (cond
     (null? set1) true
     (member? (first set1) set2) (subset? (rest set1) set2)
     :else false)))

;; Here's one way to write eqset, using rember. But I think there's a better way.
(def eqset-with-rember?
  (fn [set1 set2]
    (cond
     (null? set1) (null? set2)
     (member? (first set1) set2) (eqset-with-rember? (rest set1) (rember (first set1) set2))
     :else false)))

;; I think this will work out better: Let's rewrite eqset using subset. If they are both subsets of each other, bingo.
(def eqset-with-subset?
  (fn [set1 set2]
    (and (subset? set1 set2) (subset? set2 set1))))

;; sweet!!!

(def intersect?
  (fn [set1 set2]
    (cond
     (null? set1) false
     :else (or (member? (first set1) set2) (intersect? (rest set1) set2)))))

;; Interesting, intersect? and subset? look sort of like "opposites". Base case changes from true to false. In else clause, and changes
;;  to or. Are they opposites? One requires every member of set1 to be in set2. One requires only 1 member of set1 to be in set2.
;;  subset MUST go all the way through set1, whereas intersect can stop at the first match. Subset says "If we got all the way to
;;  empty list without violations, then return true," whereas intersect says "the minute you find something good, return true, and
;;  if we get all the way to empty list without finding something good, then return false."

;; NOTE: intersect below has NO QUESTION MARK. This FINDS THE INTERSECTION
(def intersect
  (fn [set1 set2]
    (cond
     (null? set1) ()
     (member? (first set1) set2) (cons (first set1) (intersect (rest set1) set2))
     :else (intersect (rest set1) set2))))

(def union
  (fn [set1 set2]
    (cond
     (null? set2) set1
     (member? (first set2) set1) (union set1 (rest set2))
     :else (union (cons (first set2) set1) (rest set2)))))

;; Time to write intersectall. One way I see: Process the first list. Take its first item. If it is in all of the remaining lists,
;;  include it. Then, recur on rest of the first list. That's it. But that requires a helper fn. Is there a solution that does not?
;;  What if we use intersect? Intersect the first 2, then that with the next 2, and so on. Yeah, sounds interesting. Still, seems to
;;  need a helper (or at least an accumulator, which means an overload, which you can consider a helper).
;;
;; Huh! It just made me envison reduce! He actually led me into re-inventing reduce without me realizing it up front!!!! Cool!
;;
;; So I'm going to do this his way, rather than jump ahead. I'll introduce a helper called member-all?. Takes an atom and a list of
;;  sets, and tells you whether the atom is in ALL of the sets or not.

(def member-all?
  (fn [a l-set]
    (cond
     (null? l-set) true
     (member? a (first l-set)) (member-all? a (rest l-set))
     :else false)))

(def intersectall-missed-a-good-chance
  (fn [l-set]
    (cond
     ;; What are the cases?
     ;; I'm going to shrink the first list until it is empty, so the base case is that (first l-set) is empty.
     (null? (first l-set)) ()
     ;; Each time, check that the first item of the first list is in all the other lists
     (member-all?
      (first (first l-set)) (rest l-set)) (
                                           cons (first (first l-set)) (intersectall-missed-a-good-chance (cons (rest (first l-set)) (rest l-set))))
      :else (intersectall-missed-a-good-chance (cons (rest (first l-set)) (rest l-set))))))

;; Well the good news is, I successfully wrote a correct impl. But ... his is WAY SHORTER THAN MINE! That's good news too, cuz
;;  I can learn somethihng!

;; Holy cow! It makes perfect sense! the intersectall is the intersection between the firs list and the intersectall of the rest.
;; But this hardly seems fair! We haven't DONE anything but STATE THE TRUTH, and SUDDENLY THE SHIT WORKS!

;; Yes! It harkens back to when I said I wanted the intersect of the intersect of the intersect! But I thought I could not do that
;;  without a helper! But OF COURSE YOU CAN! Just as we "remember" CONSes when we build up lists, by "remembering them on the stack",
;;  you can do that with intersect or anything else! When you want the foo of the foo of the foo, think RECURSION! It is actually
;;  pretty much the simplest recursive expression you can have!

;; Let's dewll on this point for a bit, because I missed it, and it really is a key and powerful insight. All book long, I've been
;;  producing the cons of the cons of the cons of the cons of 5 (). I've been doing it with recursion. So why, when I needed the
;;  intersect of the intersect of the intersect of (something), why didn't that occur to me? How can I make that occur to me?

;; Well, I wanted to intersect list 1 and list 2, and then take that result and intersect it with list 3, and so on. I even pictured
;;  the clojure function "iterate". But I didn't think I could do that without a helper or iterator. Something would have to
;;  "remember" the "intermediate" result that I was working with. But you can almost think of this as like a macro. What do I mean,
;;  "like a macro"? I mean a series of substitutions. That's all it is. It ultimately produces something concrete and clear.
;;  For example, building a list ACTUALLY PRODUCES something like this:
;; (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 ())))))
;; And interestingly, that is not "how you build the list '(1 2 3 4 5)". It is WHAT THE LIST '(1 2 3 4 5) ACTUALLY IS. "(1 2 3 4 5)" is
;; merely SHORTHAND.

;; Not only coudl recursion do the thing I wanted ... that pretty much is the EASIEST thing to do with recursion!

;; So "intersectall" is merely the "shell" for repeated calls to intersect! intersectall is the "listified version" of intersect. And
;;  simply by reading the requirements, you knew that! Well, turns out, you can implement it EXACTLY THAT WAY!

(def intersectall
  (fn [l-set]
    (cond
     (null? (rest l-set)) (first l-set)
     :else (intersect (first l-set) (intersectall (rest l-set))))))

;; We didn't have to DO anything because we ALREADY HAD a helper called "intersect" that properly calculated the intersect of 2 lists.
;;  We merely needed to let it "surf a wave" of lists and "do its thang". "Surf a wave of lists and do your thang" is another way of
;;  saying RECURSION!!!!!!!!!!!

(def a-pair
  (fn [sexp]
    (cond
     (atom? sexp) false
     (null? sexp) false
     (null? (rest sexp)) false
     :else (null? (rest (rest sexp))))))

(def ls-first
  (fn [pair]
    (first pair)))

(def ls-second
  (fn [pair]
    (first (rest pair))))

(def build
  (fn [a b]
    (cons a (cons b ()))))

;; third makes no sense in the context of pairs. So it may be a trick question. But let's write third for a list as 1 liner.
(def ls-third
  (fn [xs]
    (first (rest (rest xs)))))

;; Appears that a relation is a set of pairs.
;; A function is (from math class) a relation that does not repeat anything in its firsts (function means one to one). So
;; that means a function is a relation whose firsts are a set.

(def fun?
  (fn [l-rel]
    (zet? (firsts l-rel))))

;; How do we represent a finite function? With a relation mapping the input to the output.

(def revrel
  (fn [l-rel]
    (cond
     (null? l-rel) ()
     :else (cons (cons (first (rest (first l-rel))) (list (first (first l-rel)))) (revrel (rest l-rel))))))

;; Of course! He went ahead and used second and first and build to GREATLY improve readability! Anytime things get hairy in the
;;  precise manner they did above, look for helper fns!

(def revrel-readable
  (fn [l-rel]
    (cond
     (null? l-rel) ()
     :else (cons (build (ls-second (first l-rel)) (ls-first (first l-rel))) (revrel-readable (rest l-rel))))))

(def revpair
  (fn [rel]
    (build (ls-second rel) (ls-first rel))))

(def revrel-with-revpair
  (fn [l-rel]
    (cond
     (null? l-rel) ()
     :else (cons (revpair (first l-rel)) (revrel-with-revpair (rest l-rel))))))

;; fullfun means it is a valid fun, and also, the seconds are a set.

(def fullfun
  (fn [l-rel]
    (and (fun? l-rel) (zet? (firsts (revrel l-rel))))))

;; So, he wrote a helper called seconds. Then used it. as follows:

(def seconds
  (fn [rel]
    (cond
     (null? rel) ()
     :else (cons (second (first rel)) (seconds (rest rel))))))

(def fullfun-with-helper
  (fn [rel]
    (zet? (seconds (rel)))))

;; Shit damn! He figured out an even shorter way to write fullfun (aka one-to-one). Just revrel it, and see if that's a fn.
(def one-to-one?
  (fn [l-rel]
    (fun? (revrel l-rel))))

;; Chapter 8: Lambda the Ultimate! Sweeeet.
;;
;; Review: equal? is like eqan? except that it works on either atoms or lists (aka, "any sexp"). Then, at end of chapter 5,
;;  we re-wrote rember to use equal? so that it also now works on a list that can contain atoms or lists.

;; From looking back, we didn't appear to revise insertL* at the end of chapter 5, though we DID write InsertL* for the first time
;;  in around the middle of chapter 5.

;; eq? is the scheme primitive that compares non-numeric atoms for equality. equal? is something we wrote that can handle atoms
;;  or lists, but still, only for non-numerics.

;; eq? is the scheme primitive that compares non-numeric atoms for equality. equal? is something we wrote that can handle atoms
;;  or lists, and ALSO, numerics or non-numerics.

;; From looking back, we didn't appear to revise insertL* at the end of chapter 5, though we DID write InsertL* for the first time
;;  in around the middle of chapter 5.
;;
;; OK, I see what he's getting at. He wants rember-f. It is just like rember except that you pass it the fn you want to use for
;; equality. So you could pass it eq? if you only expected a list of atmos. You could pass equal? if you had a list of sexps.

;; Pretty simple, let's do it.

(def rember-f
  (fn [f? a xs]
    (cond
     (null? xs) ()
     (f? (first xs) a) (rest xs)
     :else (cons (first xs) (rember-f f? a (rest xs))))))

(def eq?-c
  (fn [a]
    (fn [x]
      (= x a))))

(def eq?-salad
  (eq?-c 'salad))

(def rember-f-curry
  (fn [f?]
    (fn [a xs]
      (cond
       (null? xs) ()
       (f? (first xs) a) (rest xs)
       ;; :else (cons (first xs) (rember-f-curry )) ;; Ahh!!!! Tricky! We need to recurse here to our unnamed inner fn!
       ;; in clojure, I can merely name the inner fn. But that is not what he'd do in scheme, so I won't do it. That way he
       ;; gets the chance to teach me what he wants to teach me.

       ;; So without using the clojure feature, and without knowing the y combinator he is about to teach me, the only thing
       ;; I can think of to do is to recurse to rember-f-curry with the same f?, and then call it. Will that work?
       :else (cons (first xs) ((rember-f-curry f?) a (rest xs)))))))

;; Yes, of course that works. We had to call rember-f-curry AGAIN with the same param it had been called with, in effect to
;;  "get ourself" again. Too bad we could not use our "self" or "current context" somehow.

(def insertL-f
  (fn [f?]
    (fn [new old lat]
      (cond
       (null? lat) ()
       :else (cond
              (f? (first lat) old) (cons new (cons old (rest lat)))
              :else (cons (first lat) ((insertL-f f?) new old (rest lat))))))))

(def insertR-f
  (fn [f?]
    (fn [new old lat]
      (cond
       (null? lat) ()
       :else (cond
              (f? (first lat) old) (cons old (cons new (rest lat)))
              :else (cons (first lat) ((insertR-f f?) new old (rest lat))))))))

;; OK, this is a cool challenge, I'm going to nail it. Can I write insert-g, which is capable of inserting to the right or
;;  the left.
;;
;; Two ways come to mind. First, if they pass in 'left or 'right, I can easily do it. But better, let them pass in a fn.
;;  The first impl that comes to mind is that they pass me a fn that takes (rest lat). They can then cons new and old in
;;  whatever order they want. (Hell, they could even do something entirely different).

(def insertG-fn-with-test
  (fn [f? tfn]
    (fn [new old lat]
      (cond
       (null? lat) ()
       :else (cond
              (f? (first lat) old) (tfn new old (rest lat))
              :else (cons (first lat) ((insertG-fn-with-test f? tfn) new old (rest lat))))))))

;; I'm going with that. To let them pass in 'left or 'right means 2 new hardcoded symbols. There's no value to that over just having
;;  2 different functions, one with "left" in the name and the other with "right".
;; So I have earned some coffee cake, but we don't have any!

(def fn-for-subst
  (fn [new old xs]
    (cons new xs)))

;; Before going further ... he quit parameterizing the test. So here's insertG-fn with eq hardcoded:

(def insertG-fn
  (fn [tfn]
    (fn [new old lat]
      (cond
       (null? lat) ()
       :else (cond
              (eqan? (first lat) old) (tfn new old (rest lat))
              :else (cons (first lat) ((insertG-fn tfn) new old (rest lat))))))))

(def subst-with-insertG
  (fn [new old l]
    ((insertG-fn fn-for-subst) new old l)))

;; OH BUT WAIT A MINUTE!!! Apparently you CAN do something like point-free style using clojure! See, insertG-fn can be called with
;;  JUST ONE ARG. And it returns a fn. So subst-with-insertG can merely bind to that fn, rather than being a new fn itself that
;;  Calls that fn!

(def subst-with-insertG-much-better
  (insertG-fn fn-for-subst))

;; In his (yyy) (which is rember-with-insertG actually), he passes false for "new". But nothing ever uses new, so false is just a
;;  placeholder. I'd use _ in that case.

(def atom-to-fn
  (fn [a]
    (cond
     (eqan? a '+) plus
     (eqan? a '*) mult
     (eqan? a '%) power
     :else nil)))

(def value-abstract
  (fn [aexp]
    (cond
     (atom? aexp) aexp
     :else ((atom-to-fn (operator (aexp))) (first (rest aexp)) (first (rest (rest aexp)))))))

(def value-abstract-with-helpers
  (fn [aexp]
    (cond
     (atom? aexp) aexp
     :else ((atom-to-fn (operator aexp)) (value-abstract-with-helpers (first-sub-exp aexp)) (value-abstract-with-helpers (second-sub-exp aexp))))))

(def multirember-f
  (fn [test?]
    (fn [a lat]
      (cond
       (null? lat) ()
       (test? (first lat) a) ((multirember-f test?) a (rest lat))
       :else (cons (first lat) ((multirember-f test?) a (rest lat)))))))

;; Cool. Now, he wants to combine the "test" and "a" params. So we'll pass in a predicate.

(def multiremberT
  (fn [pred?]
    (fn [lat]
      (cond
       (null? lat) ()
       (pred? (first lat)) ((multiremberT pred?) (rest lat))
       :else (cons (first lat) ((multiremberT pred?) (rest lat)))))))

;; understanding multirember&co
;;
;; col is a fn that takes 2 lists. We have not been given info about what it does with them.
;; If the element-under-test matches a, then       : (col newlat (cons (first lat) seen))
;; If the element-under-test does not match a, then: (col (cons (first lat) newlat) seen)
;;
;; So, the element-under-test gets consed onto either newlat or seen. newlat if it matches, seen otherwise.
;;
;; Here are the calls to coll for 'a '(b a c)
;; (multirember&co 'a '(b a c))

;; null? no
;; eq? 'b 'a? no
;;
;;
;; (multirember&co 'a '(a c) (fn [newlat seen] (col (cons 'b newlat) seen)))
;;
;; Does 'a = 'a? Yes:
;; (multirember&co 'a '(c) (fn [newlat seen] (col newlat (cons 'a seen))))
;;
;; In this call, a = 'a, lat = '(c)
;; does 'a = 'c? no
;;
;; (multirember&co 'a () (fn [newlat seen] (col (cons 'c newlat) seen)))
;;
;; In this call, a = 'a, lat = ()
;;
;; empty? Yes.
;;
;; ACTUALLY CALL col! Pass it 2 empty lists:

;; pass () () to (fn [newlat seen] (col (cons 'c newlat) seen))
;;
;; newlat = (), seen = ().
;;
;; It calls "col", which is the PREVIOUS value of col.
;;
;; (col (cons 'c ()) ())
;;
;; So pass '(c) and () to: (fn [newlat seen] (col newlat (cons 'a seen)))
;;
;; (col '(c) (cons a ()))
;; Simplifies to
;; (col '(c) '(a))
;;
;; Of course, col here is the "previous previous" version, so...
;; Pass '(c) '(a) to: (fn [newlat seen] (col (cons 'b newlat) seen))
;;
;; (col (cons 'b '(c)) '(a))
;; simplifies to
;; (col '(b c) '(a))
;;
;; So ultimately, it's going to call the fn I pass in ONE TIME. And it will pass me '(b c) '(a).
;;
;; It is telling me what the newlat will be, and the list of things it has seen. I can then return whatever I want, and whatever
;;  I return will be the result returned from multirember&co. Pretty cool, but hopefully it will get easier to understand.
;;
;; "col" stands for COLLECTOR, and Collectors are sometimes referred to as CONTINUATIONS!!!

;; The Tenth Commandment -- I didn't fully get it at first. But now I see. "Build functions to collect more than one value at a time."
;;  There are TWO KEY parts: "build" -- you're building new functions that use the old ones, and "more than one value at a time"
;;
;; If you only needed to collect or build up ONE VALUE, you wouldn't need fns. We've been doing that kind of thing all book long.
;;  If you need to collect MULTIPLE values, you can "build" up functions to acheive that.

;; multiinsertLR -- If I understand correctly, it expects oldL and oldR to be DIFFERENT. When it finds oldL, it inserts new to the
;;  left. When it finds oldR, it inserts new to the right.
;;
;; Before starting, it doesn't SEEM like I'll need to build fns to collect more than one value. But I suspect he's leading me to need
;;  that. So let's see what happens.

(def multiinsertLR
  (fn [oldL oldR new lat]
    (cond
     (null? lat) ()
     (eqan? (first lat) oldL) (cons new (cons oldL (multiinsertLR oldL oldR new (rest lat))))
     (eqan? (first lat) oldR) (cons oldR (cons new (multiinsertLR oldL oldR new (rest lat))))
     :else (cons (first lat) (multiinsertLR oldL oldR new (rest lat))))))

;; nailed it.

;; Yes, multiinsertLR&co would need one additional arg, the collection fn. Of course, there are probably all kinds of idioms for
;;  collectors/continuations that I don't know.

;; He says: "When  multiinsertLR&co is done, it will  use col  on  the  new lat, on  the  number of left
;; insertions, and  the number of ri ght insertions.  Can you  write an outline of multiinsertLR&co?"

;; I interpret that statement as: "At the very end, multiinsertLR&co will call your collector fn, and when it does, it will pass
;;  the new lat, the number of left insertions, and the number of right insertions". Surprising to me that we're only capturing
;;  the NUMBER of left/right insertions, instead of the actual values inserted or their position or something. But those are the
;;  requirements.

(def multiinsertLR&co
  (fn [oldL oldR new lat col]
    (cond
     (null? lat) (col () 0 0) ;; the base case is 0 0 instead of () (), because the thing we're building up is numbers, not lists
     (eqan? (first lat) oldL) (multiinsertLR&co oldL oldR new (rest lat)
                                                (fn [newlat nleft nright]
                                                  (col (cons new (cons oldL newlat)) (add1 nleft) nright)))
     (eqan? (first lat) oldR) (multiinsertLR&co oldL oldR new (rest lat)
                                                (fn [newlat nleft nright]
                                                  (col (cons oldR (cons new newlat)) nleft (add1 nright))))
     :else (multiinsertLR&co oldL oldR new (rest lat)
                             (fn [newlat nleft nright]
                               (col (cons (first lat) newlat) nleft nright))))))

;; Next up, write "even?" and then use it to write evens-only, which takes a list OF LISTS of numbers, and removes all the odds.

(def ls-even?
  (fn [a]
    (cond
     (zero? a) true
     :else (not (ls-even? (sub1 a))))))

(def evens-only*
  (fn [xs]
    (cond
     (null? xs) ()
     (atom? (first xs)) (cond
                         (ls-even? (first xs)) (cons (first xs) (evens-only* (rest xs)))
                         :else (evens-only* (rest xs)))
     :else (cons (evens-only* (first xs)) (evens-only* (rest xs))))))

;; next up! evens-only*&co: "It  builds a nested list of even  numbers  by removing the odd ones from  its  argument
;; and simultaneously multiplies the even numbers and sums up the odd  numbers  that occur in its  argument.

;; So it only takes 2 params: the list of nested lists and the collector. It removes odds, and it also multiplies evens and sums up
;;  odds. Pretty easy really. col will take 3 params: newlat, even-product and odd-sum. And doing this is jsut like what we've been
;;  doing all book, except it builds up multiple values, and therefore it uses a function with multiple params.

(def evens-only*&co
  (fn [xs col]
    (cond
     (null? xs) (col () 1 0)
     (atom? (first xs)) (cond
                         (ls-even? (first xs)) (evens-only*&co (rest xs)
                                                               (fn [newxs ep os]
                                                                 (col (cons (first xs) newxs) (mult ep (first xs)) os)))
                         :else (evens-only*&co (rest xs)
                                               (fn [newxs ep os]
                                                 (col newxs ep (plus os (first xs))))))
     ;; OK, trickiness here!
     ;; I deliberated on this for a long time.
     ;; I need to process first, then process rest. But that will get me 2 buckets (assuming I have 2 lists). If I have x lists
     ;; that will get me x buckets. And I don't see an elegant way to combine multiple collectors (aka buckets) together.
     ;;
     ;; Then I realized what we need to do is create 1 bucket, and then use that for first, and then for rest. It all just goes
     ;;  into 1 bucket. The first answer that pops into my head for that is of course to use a let. But we don't have "let" in this
     ;;  "Little Schemer" world. However, I happen to know about "let over lambda". So I can make an outer fn that takes a collector,
     ;;  and call that fn with the one bucket. That fn could then use it to process first and rest. By george, I think it will work.
     ;;
     ;; I wrote the following, then decided I don't think that's right. I think I need one call to col, and that one call needs to
     ;;  join the lists of first and rest. So I'll try that below.
     ;; :else ((fn [one-bucket]
     ;;          (evens-only*&co (first xs) one-bucket)
     ;;          (evens-only*&co (rest xs) one-bucket)) (fn [newxs ep os]
     ;;                                                   (col newxs ep os))))))
     ;;
     ;; So, can my else clause consist only of a call to col, with the properly combined results?
     ;;
     ;; :else (col (evens-only*&co (first xs) ???) )
     ;;
     ;; But I don't know where to go with this. Now I think I need let over lambda with 3 params:
;;      :else ((fn [newxs ep os lcol]
;;               (evens-only*&co (first xs) lcol)
;;               (evens-only*&co (rest xs) lcol)) () 1 0 (fn [o-newxs o-ep o-os]
;;                                                         (col (cons o-newxs (list newxs)) (mult o-ep ep) (plus o-os os)))))))

;; ;;
;; Now that's the best I got so far, but I'm pretty sure something's not right. Why did I have to essentially "manually" make my
;;  own 3 "local variables" to do the collecting, when the collector/continuation idiom should do it for me? Even if this works, I
;;  have most likely worked too hard. So there will be something excellent to learn when I read on.
;;
     ;; :else ((fn [newxs ep os]
     ;;          (println "newxs: " newxs " ep: " ep )
     ;;          (evens-only*&co (first xs) (fn [o-newxs o-ep o-os]
     ;;                                       (col (cons o-newxs (list newxs)) (mult o-ep ep) (plus o-os os))))
     ;;          (evens-only*&co (rest xs) (fn [o-newxs o-ep o-os]
     ;;                                      (col (cons o-newxs (list newxs)) (mult o-ep ep) (plus o-os os)))) () 1 0)))))

;; OK, 2 things here: (1) It is not working, tells me I sent zero args to evens-only*&co, but I don't see where. (2) When a single
;;  list is processed by evens-only*&co, it returns a list. So why can't I cons the results -- AHA! That's incorrect! When a single
;;  list is processed by evens-only*&co, it RETURNS WHATEVER COL RETURNS! For one thing, it is THREE VALUES (a triple), not a list.
;;  For another thing, that triple is not RETURNED. That triple is PASSED TO THE COL FUNCTION.
;;
;; THIS IS WHERE THE FACT THAT IT IS A CONTINUATION COMES INTO PLAY!!! It will process the list and then CALL ME WHEN IT IS DONE.
;; So how can I make use of that? Perhaps the continuation ITSELF is where we can process the rest of the list? And if we do,
;;  we can merely use the STACK to "combine" the items. YES! Let's see where that gets us.
;;
     :else (evens-only*&co (first xs)
                           (fn [newxs ep os]
                             (evens-only*&co (rest xs) (fn [r-newxs r-ep r-os]
                                                         (col (cons newxs r-newxs) (mult r-ep ep) (plus r-os os)))))))))
;; WHOO HOO! WOOT! It is WORKING!
;; I haven't looked at his solution yet, but this is GIVING ME CORRECT RESULTS!
;;
;; The key seems to have been to use the continuation for recurrence. That gave me the "starting bucket variables", so no need for
;;  let over lambda. Rather than relying on what each call to evens-only*&co RETURNED, I relied on WHAT IT PASSED TO MY CONTINUATION.
;;  That enabled me to process the rest of the list INSIDE THE CONTINUATION, and it gave me one lexical scope where I could collect
;;  everything into one set of variables. That made it convenient to fulfill my final obligation, which was passing the variables
;;  that had collected everything to the original caller's "col" fn.

;; Note: the author seriously glossed over how difficult it was to come up with the right answer. If I was merely reading, I'd feel
;;  smug and confident about my knowledge! (Dunning Kruger at its best). I wish he had spent more time explaining ... but still, you
;;  cannot learn something if you don't first know your own ignorance about it! That's why exercises / tests / challenges / building
;;  real systems is VITAL. You need REALITY to tell you whether or not you really know something! If I don't test my own knowledge
;;  when I learn something new, then I'm going to get the bad results that come from THINKING I know something but finding out
;;  the hard way that I don't! Anytime you learn something new, PROVE IT! And the more complex it is, the greater the required proof!

;; Looking again at evens-only*&co, you NEVER call the collector from anywhere other than INSIDE THE COLLECTOR, EXCEPT FOR ONE case:
;;  you call the collector from the base case.

;; *********************** CHAPTER 9
;;

(def looking
  (fn [a lat]
    ((fn cheating-with-clojure [a index lat orig-lat]
       (cond
        (null? lat) false
        (one? index) (cond
                      (number? (first lat)) (cheating-with-clojure a (first lat) orig-lat orig-lat)
                      :else (eqan? (first lat) a))
        :else (cheating-with-clojure a (sub1 index) (rest lat) orig-lat)) ;; I need to recur on my lambda. Clojure lets me name it.
       ) a 1 lat lat))) ;;                                                  Scheme does not let me do that (hence "cheating")

(def keep-looking
  (fn [a the-value lat]
    (cond
     (number? the-value) (keep-looking a (pick the-value lat) lat)
     :else (eqan? the-value a))))

(def his-looking
  (fn [a lat]
    (keep-looking a (pick 1 lat) lat)))


;; His is more elegant than mine because (1) he used a helper fn "pick", and (2) he therefore was able to pass the whole list around
;;  at all times.

;; NICE! Not only did I write my own (a bit clumsy) looking , I also wrote keep-looking when he first asked, which he then said
;;  HE DID NOT EXPECT ME TO KNOW!

;; Couple notes:
;; (1) he uses "sorn" to stand for symbol or number
;; (2) keep-looking is an example of UNNATURAL RECURSION. It does NOT recur on the list. It can do that because: (a) the helper does
;;  some recurringn on the list for us, and (b) the original requirements require us to start back with the whole list from time to \
;;  time.

;; He points out that, because keep-looking usses unnatural recursion, IT MIGHT NOT EVER STOP! If one index points to another index
;;  which points back to the first index, then INFINITE LOOP, stack overflow.
;;
;; "TOTAL FUNCTIONS" -- they are valid for any inputs
;; "PARTIAL FUNCTIONS" -- (NOT "partial application") -- not valid for all inputs.

;; What is the most unnatural recursion possible?
(def eternity
  (fn [x]
    (eternity x)))
;;
;; It is "as unnatural as possible" because it never makes any progress towards a base case. It is the "most partial function" possible

;; shift:
;; "The function shift  takes a pair whose first component is a pair and builds a pair by
;; shifting the second part of the first component into the second component."
(def shift
  (fn [outer-pair]
    (build (ls-first (ls-first outer-pair)) (build (ls-second (ls-first outer-pair)) (ls-second outer-pair)))))

;; (def atoms-in-pair
;;   (fn [pair]
;;     (cond
;;      (atom? (ls-first pair)) (cond
;;                               (atom? (ls-second pair)) 2
;;                               :else (add1 (atoms-in-pair (ls-second pair))))
;;      :else (plus (atoms-in-pair (ls-first pair)) (atoms-in-pair (ls-second pair)))))))

(def atoms-in-pair
  (fn [x]
    (cond
     (nil? x) 0
     (atom? x) 1
     :else (plus (atoms-in-pair (ls-first x)) (atoms-in-pair (ls-second x))))))

;; I honestly have no idea what the fuck he is talking about with align and weight. He asks "is align a partial fn?" then answers
;; "we don't know. it looks like there might be some args it would go forever on." I'm totally with him that far. Then he says
;;  there's this fn called weight which pays double attention to the first component of align's args. Why double? What is so special
;;  about the first arg anyway?
;;
;; Then, after merely talking about this idea for a weight fn, he asks again if align is partial, and says no, as if suddenly we know
;;  the answer. WTF?

;; I can see that the args to align get simpler each time it recurses. And eventually (first pora) will not be a pair. Then we'll hit
;;  the else clause, and we'll do a (build (first pora) (align (second pora))). And that means the second member of the pair will
;;  undergo the same process. So from that standpoint, I can buy that align does not recur forever. Why didn't he say that?

;; (def will-stop?  ;; Will the fn f stop, when passed ()
;;   (fn [f]
;;     ))

;; ;; How could this possibly be done? You'd have to know something about the internals of the fn, or hook into it to see something
;; ;;  about how it behaves. Perhaps we could "hijack" the function, so that it calls us each time instead of itself. Then, we "analyze",
;; ;;  and then we call it so as to not break the recursion. Still, whose to say there's not some pattern which appears to be getting
;; ;;  simpler, only to jump back up at the end? (sub1 x), coupled with (if (= x 2) (recur on 1000)). Without knowing the fn itself,
;; ;;  you simply cannot tell.

;; ;; I think he is coding up Godel's incompleteness theorum with will-stop? and last-try.

;; ;; Now, he's addressing the problem: What if you could not name a fn? You would no longer be able to recur, because you would have
;; ;;  no way to call yourself.

;; ((fn [length]
;;    (fn [l]
;;      (cond
;;       (null? l) 0
;;       :else (add1 (length (rest l))))))
;;  eternity)

;; ;; The above is from the book. I need to dissect it. We define a fn, and call it with "eternity". DON'T GET CONFUSED -- WE ARE CALLING
;; ;;  THE *OUTER* FN! So we're binding "length" to the function "eternity". The RETURN VALUE is a new function that calls eternity
;; ;;  instead of recurring. So that fn is length0, because it only works for lists with length of 0, and has an infinite loop
;; ;;  otherwise.

;; ;; Now, rewrite length<=1 in same style. But first, let me write length<=1 the long way.

;; (def length<=1-long-way
;;   (fn [l]
;;     (cond
;;      (null? l) 0
;;      :else (add1 (
;;                   (fn [l]
;;                     (cond
;;                      (null? l) 0
;;                      :else (add1 (eternity (rest l))))) ;; So far, I don't see why we do add1 and rest. We're just blowing up.
;;                   (rest l)
;;                   )
;; ))))

;; ;; Now, let's rewrite length<=1 using that style above.
;; (def length<=1-still-not-all-the-way
;;   (fn [l]
;;     (
;;      (fn [l-fn]
;;        (cond
;;         (null? l) 0
;;         :else (add1 (l-fn (rest l)))))
;;      (fn [l2]
;;        (cond
;;         (null? l2) 0
;;         :else (add1 (eternity (rest l2))))))
;;     ))

;; ;; That's all well and good, but we want not to even use "define". So let me write it that way.
;; ;; ((fn [l-fn]
;; ;;    (fn [l]
;; ;;      (cond
;; ;;       (null? l) 0
;; ;;       :else (add1 (l-fn (rest l)))))
;; ;;    )(fn [l2]
;; ;;      (cond
;; ;;       (null? l2) 0
;; ;;       :else (add1 (eternity (rest l)))))
;; ;;  )

;; ;; Even still, he did something slightly different. He repeated that helper fn twice. So let me do that.
;; ((fn [l-fn]
;;    (fn [l]
;;       (cond
;;        (null? l) 0
;;        :else (add1 (l-fn (rest l)))))
;;    ) ((fn [g-fn]
;;       (fn [l]
;;         (cond
;;          (null? l) 0
;;          :else (add1 (g-fn (rest l))))))
;;     eternity))

;; ;; That is exactly what he's doing. Now, let's do length<=2
;; ((fn [h-fn]
;;    (fn [l]
;;      (cond
;;       (null? l) 0
;;       :else (add1 (h-fn (rest l))))))
;; ((fn [l-fn]
;;    (fn [l]
;;       (cond
;;        (null? l) 0
;;        :else (add1 (l-fn (rest l)))))
;;    ) ((fn [g-fn]
;;       (fn [l]
;;         (cond
;;          (null? l) 0
;;          :else (add1 (g-fn (rest l))))))
;;     eternity)))

;; ;; OK, on page 163 his words get confusing. He says to "name the function that makes a fn that looks like length". What he means,
;; ;;  though, is NOT to name the fn. We cannot name fns, because we are working without "define". He means to NAME THE PARAMETER
;; ;;  THAT HOLDS THE FUNCTION. In other words, he doesn't like those h-fn, l-fn and g-fn names in my example, and likewise he
;; ;;  doesn't like the name "length" or "f" and "g" in his implementations.

;; ;; That's ALL he SAYS. But then he DOES a HELLUVA LOT MORE THAN THAT. He also realizes that mk-length could call ITSELF over and over.
;; ;;  Why in God's name he would not explicitly say that to me since he's trying to teach me, I don't know. But I did notice earlier
;; ;;  that each "copy/paste" of mk-length was very similar. Still, I need to prove to myself that recurring on mk-length makes sense
;; ;;  and does what we want.

;; ;; No, wait, that is not exactly what he is doing. He REALLY DOES want to name the function. So he uses "let-over-lambda". He wraps
;; ;;  everything we already have in a lambda that takes one argument and names that arg mk-length. That lets him call it over and over.
;; ;;  He's simply using let-over-lambda to name a fn even though we don't have "define".

;; ;; Let me do that to length0:

;; (                                               ;; Line 1
;; (fn [mk-length]                                 ;; Line 2
;;   (mk-length eternity))                         ;; Line 3
;; (fn [length]                                    ;; Line 4
;;    (fn [l]                                      ;; Line 5
;;      (cond                                      ;; Line 6
;;       (null? l) 0                               ;; Line 7
;;       :else (add1 (length (rest l)))))))        ;; Line 8
;; ;; Let's dwell on this a bit. I've numbered the lines for discussion.
;; ;; Lines 4-8 are the "helper" fn that is core to what we're doing now. We wanted to name it, so we introduced lines 1-3. Lines 1-3
;; ;;  assign the name "mk-length" to the fn defined on line 4. Since we now have a name, it is possible for recursion to be used.
;; ;;
;; ;; Don't be confused though: the only reason the fn on line 4 is named "mk-length" is that we passed the fn on line 4 as a parameter
;; ;;  to the fn on line 2. Straight up "let over lambda".
;; ;;
;; ;; But WAIT! Lines 1-2 also do ONE MORE THING. Not only do they assign the name "mk-length" to the fn defined on line 4. They also
;; ;;  CALL that fn, passing "eternity" to it. So the entire code block (lines 1-8) actaully return Length0 (and testing confirms
;; ;;  this).
;; ;;
;; ;; Now, what he's saying is that, since we have named the pattern, a lot of our ugly repetition can go away. He claims that this
;; ;; is length<=1:
;; ((fn [mk-length]
;;    (mk-length                   ;; See right here, he calls mk-length twice (calls it on itself).
;;     (mk-length eternity)))
;;  (fn [length]
;;    (fn [l]
;;      (cond
;;       (null? l) 0
;;       :else (add1 (length (rest l)))))))

;; ;;
;; ;; I think the key claim there is that, my helper fn (defined on Line 4 of the numbered example earlier), can be called on itself.
;; ;; You can pass a value to "length-fn" that IS length-fn. If we hadn't named it, we'd have to repeat its definition (which is exactly
;; ;;  what we did earlier with length0, length<=1 and length<=2). We simply named the pattern we wanted to repeat, and now we're using
;; ;;  the name. That's really all we're doing.

;; ;; And therefore, here's length<=2:
;; ((fn [mk-length]
;;    (mk-length (mk-length (mk-length eternity))))
;;  (fn [length]
;;    (fn [l]
;;      (cond
;;       (null? l) 0
;;       :else (add1 (length (rest l)))))))

;; ;; And here's length<=3:
;; ;; ((fn [mk-length]
;; ;;    (mk-length (mk-length (mk-length (mk-length eternity)))))
;; ;;  (fn [length]
;; ;;    (fn [l]
;; ;;      (cond
;; ;;       (null? l) 0
;; ;;       :else (add1 (length (rest l)))))))

;; ;; Alright. He is saying, if we had an infinite tower of these, we could handle any list. In fact, in practicality, we don't need an
;; ;;  *infinite* tower ... we just need one long enough for the current list were being asked about. He then says, what if we could
;; ;; take a guess. If the list is shorter or equal to our guess, great! If we guess low, then we're screwed. But what if, in addition
;; ;;  to guessing, we could ALSO find out we guessed too low, and do a "just-in-time" increase of our guess? The idea is that
;; ;;  we don't pass "eternity" to mk-length at first. Instead, we pass "mk-length" itself to mk-length. We'd get back a fn that
;; ;;  still takes 1 param called "mk-length". After calling what we passed in, it would then call the param we passed, which is
;; ;;  mk-length. So we could pass "eternity" to THAT. In other words, pass eternity to the outer mk-length, which then calls the inner
;; ;;  mk-length. This would create a stack of mk-length calls that would eventually unravel to "eternity".

;; ;; I don't totally follow him to the next step, however. He simply takes length0 as we wrote above, and replaces "eternity" with
;; ;;  mk-length. And he claims that it is STILL length0. OHHH, I GET IT! Of COURSE it is still length0. It doesn't matter if you
;; ;;  call eternity, or (iterate true), or whatever the hell you want! You're entering an infinite loop however you please. Well, we
;; ;;  desire our infinite loop to be calling mk-length over and over and over. So the fn will still blow up if length is > 1, and for
;; ;;  lengths of 1 or 0 it will work just fine. GOT IT.

;; ;; Then, he says, "let's rename the param from 'length' to 'mk-length' just to remind ourselves that we're passing mk-length to
;; ;;  mk-length." Cool, no worries.

;; ;; OK, I get it. Once you get past all your hardcoded repeated calls to mk-length (the ones that look like this:)
;; ;;    (mk-length (mk-length (mk-length mk-length)))
;; ;; you then call eternity. Of course, this causes a stackoverflow. The whole goal is to "guess high enough" that you never hit the
;; ;;  stack overflow.
;; ;;
;; ;; THEN, he makes the next jump: YOU NEVER CALL ETERNITY AT ALL. Right at the point you would have called eternity, merely call
;; ;;  mk-length again instead. Call it on the (rest ) of the list, and add1 to it. You are indeed setting up infinite recursion, except
;; ;;  that earlier in the recursion, you set up an "exit strategy". You will eventually hit zero and bottom out.
;; ;;
;; ;; Here's his implementation of mk-length:

;; ;; ((fn [mk-length]
;; ;;    (mk-length mk-length))
;; ;;  (fn [mk-length]
;; ;;    (fn [l]
;; ;;      (cond
;; ;;       (null? l) 0
;; ;;       :else (add1
;; ;;              ((mk-length mk-length)
;; ;;               (rest l)))))))

;; ;; So, I think the y-combinator (which he is leading up to) comes down to these few principles:
;; ;; 1. It is ok to call yourself in what superfically appears to be infinite recursion if you have a base case that stops the recursion.
;; ;; 2. Calling yourself is hard when you cannot name yourself.
;; ;; 3. But you can find a pattern for calling yourself using let over lambda.
;; ;;
;; ;; Let me see if I can use those principles to work out the definition for any-num? anynum? is a fn which tells you if any of the
;; ;;  members of a list of atoms is a number.

;; ;; First, here's the version using "def"
;; ;; (def any-num-using-def?
;; ;;   (fn [lat]
;; ;;     (cond
;; ;;      (null? lat) false
;; ;;      (number? (first lat)) true
;; ;;      :else (any-num-using-def? (rest lat)))))

;; ;; Now, I don't have def. I need to return a fn that does the job. So I will have a fn that returns it:

;; ;; (fn []
;; ;;   (fn [lat]
;; ;;     (cond
;; ;;      (null? lat) false
;; ;;      (number? (first lat)) true
;; ;;      :else (what-to-put-here? (rest lat))))) ;; See, when it comes time to recur, I don't yet know what to put. Other than that, good.

;; ;; In order to be able to put something there that would recur, I need to name that inner fn via let-over-lambda. Like this:
;; ;; ((fn [the-inner-fn-that-needs-name]
;; ;;    (identity the-inner-fn-that-needs-name) ;; some parameters here)
;; ;;   )
;; ;;    (fn [lat]
;; ;;      (cond
;; ;;       (null? lat) false
;; ;;       (number? (first lat)) true
;; ;;       :else (the-inner-fn-that-needs-name (rest lat)))))

;; ;; Hmmm, this is totally different from what he did, but looks like it might work. If not, I need to understand why not.
;; ;; Well, I see why not ... but the idea may still be sound. Even though I used let-over-lambda to name the inner fn, that name
;; ;;  is not in scope where I need to call it. How was that not a problem in his appraoch?

;; ;; AHH, I SEE. He DID NOT name that inner fn. He named the OUTER fn that wraps the inner fn. That fn takes a fn to call when
;; ;;  recursion occurs. Let me try that:

;; ;; ((fn [what-to-call-when-you-recur]
;; ;;    (what-to-call-when-you-recur what-to-call-when-you-recur))
;; ;;  (fn [lat]
;; ;;    (cond
;; ;;     (null? lat) false
;; ;;     (number? (first lat)) true
;; ;;     :else ((what-to-call-when-you-recur what-to-call-when-you-recur) (rest lat)))))

;; ;; No, that still won't compile, for the same reason. what-to-call-when-you-recur is STILL not in scope.
;; ;;
;; ;; A THIRD FN is needed! I have been trying with 2 fns so far: the inner one that actually checks for numbers, and the outer one
;; ;;  which serves as the let-over-lambda.
;; ;;
;; ;; I need a THIRD fn, which "makes" the fn that does the work. So here's how the 3 fn's I need break down:
;; ;; (1) I need the fn which does the work.
;; ;; (2) I need a "maker fn". It makes and returns the fn that does the work.
;; ;; (3) I need to NAME the "maker fn". To do that, I use let-over-lambda, which means I must introduce the 3rd fn and call it. (what
;; ;;   the javascript world used to call an "immediately executing fn").

;; ;; So, here goes:

;; ;; ((fn [maker] ;; let-over-lambda fn
;; ;;    (maker))
;; ;;  (fn [] ;; this is the maker
;; ;;    (fn [lat] ;; worker fn
;; ;;      (cond
;; ;;       (null? lat) false
;; ;;       (number? (first lat)) true
;; ;;       :else ?????)))) ;; What to put here for the recursion???

;; ;; Now, we're on the right track. You need the maker because you're going to make a new one over and over again till you get the
;; ;;  answer you need. You need the worker to do the work (duh). You need the outer for let-over-lambda (merely to assign a name
;; ;;  to maker).

;; ;; Now, the ???? indicates we don't know what to put for the recursion! Well, we want to call maker again
;; ;;  to make a new fn that we will recurse on. That will
;; ;;  "make" a new worker fn for us. And when that new worker fn recurses, we want IT to call maker. This means that when it comes
;; ;;  time to recurse, we're going to call maker, and pass maker. Let's do it:

;; ;; ((fn [maker]
;; ;;    (maker))
;; ;;  (fn []
;; ;;    (fn [lat]
;; ;;      (cond
;; ;;       (null? lat) false
;; ;;       (number? (first lat)) true
;; ;;       :else ((maker) (rest lat)))))) ;; Read Carefully! We call maker, and maker returns a fn. We then call THAT fn with (rest lat)
;; ;;
;; ;; There is ONE REMAINING PROBLEM. We can't call maker in the else clause, because maker is not in scope. We get it into scope by
;; ;;  passing it in as a parameter, and propogating that on any subsequent calls to maker too. Like this:
;; ;; (
;; ;; ((fn [maker]
;; ;;    (maker maker))          ;; Change # 1: pass maker, so that the inner fn can call it.
;; ;;  (fn [maker]               ;; Change # 2: accept maker as an arg
;; ;;    (fn [lat]
;; ;;      (cond
;; ;;       (null? lat) false
;; ;;       (number? (first lat)) true
;; ;;       :else ((maker maker) (rest lat)))))) ;; Change 3: when we call maker to recur, pass maker to it.
;; ;; '(a s d f g h j k l 42))
;; ;; So, does the above work as an impl of any-number? YES IT DOES!!!

;; ;; So perhaps this is really just another use of continuations! When it is time to recur, you call the fn I give you, and I will
;; ;;  give you something just like yourself. I don't know whether it is continuations or not.

;; ;; But the heart of the pattern seems to be the maker. It makes a worker that recurs by calling maker again. Thereby, it pretty much
;; ;;  "calls itself" without being able to name itself. This is clearly heavy duty use of first-class functions.

;; ;; Another aha -- essentially, the y-combinator is not much different from a lazy sequence. It is defined in such a way that it
;; ;;  recurs forever, but it ends up only doing as much as you pull from it. So as long as you ask it a finite question, it gives
;; ;;  you a finite answer. In this case, the "lazy" part is the next call to "maker", which only happens if you really need it.
;; ;;  This kind of thing helps illustrate why a Haskell fn defined one way succeeds against an infinite list, but fails when slightly
;; ;;  tweaked and applied to an infinite list. Is the "infinite part" totally encapsulated in the "lazy part"? If so, you're good to
;; ;;  go. If not you're screwed.

;; ;; So then, he proposes a tweak to his "lenght" fn. He tries to apply the name "length" to the length fn for readability. But he
;; ;;  fails. His result recurs forever because the infinite part is no longer inside the lazy part. Just to make sure I'm
;; ;;  understanding him correctly, let me type it in and try it.
;; ((fn [mk-length]
;;    (mk-length mk-length))
;;  (fn [mk-length]
;;    ((fn [length]
;;       (fn [l]
;;         (cond
;;          (null? l) 0
;;          :else (add1 (length (rest l))))))
;;     (mk-length mk-length))))

;; ;; Confirmed. Merely evaluating that code causes a stack overflow. So he's pointing out the distinction between the "lazy part" and
;; ;;  the non-lazy part, and also pointing out that the infinite part needs to go in the lazy part.
;; ;;
;; ;; Another thing going on here that caught me by surprise: when just executing that code snippet caused a stack overflow, for a sec
;; ;;  that surprised/ confused me. I thought, "I'm just defining something, why would that stack overflow?" But of course, I'm executing
;; ;;  code. Sort of how in Ruby even definitions are executed code, likewise here. I knew that, of course, because I just made reference
;; ;;  to the fact that we were declaring and calling an "immediately executing function". But I temporarily forgot it.

;; ;; So, now he wants to still "get back a fn that looks like length", but without breaking things as he did on his first try.
;; ;; He wants to turn (mk-length mk-length) into a fn. So he simply wraps it in a fn. Like this:
;; ;; (fn [x]
;; ;;   ((mk-length mk-length) x))
;; ;;
;; ;; legal enough.
;; ;;
;; ;; So length becomes:
;; ((fn [mk-length]
;;    (mk-length mk-length))
;;  (fn [mk-length]
;;    (fn [l]
;;      (cond
;;       (null? l) 0
;;       :else (add1
;;              ((fn [x]
;;                 ((mk-length mk-length) x))
;;              (rest l)))))))
;; ;;
;; ;; Works fine. But so far, we have only obfuscated things a bit from the previously working version of length. But that's prepared
;; ;;  us for the next step. Now that (mk-length mk-length) is wrapped in a fn, we can pass that wrapping fn as a param. So we can
;; ;;  insert a NEW fn (yes, a FOURTH one) that takes our new wrapping fn as a param. The new (fourth) fn will look like "length".
;; ((fn [mk-length]
;;    (mk-length mk-length))
;;  (fn [mk-length]
;;    ((fn [length-fn]                               ;; Next, he asks if we can extract this fn right here and give it a name.
;;       (fn [l]
;;         (cond
;;          (null? l) 0
;;          :else (add1 (length-fn (rest l))))))
;;     (fn [x]
;;       ((mk-length mk-length) x)))))

;; ;; Note comment above. His next question is, can we extract the fn that starts on the line commented above and give it a name.
;; ;;  His answer is "yes", and further he notes that it does not depend on mk-length at all.

;; ;; So I guess what he has been striving for is a fn that looks like length and does not have any "orthogonal" influences. It is
;; ;;   not "polluted" by all this "infrastructure" we have to put around it. Let's him fully separate infrastructure from
;; ;;  (pardon the next term) "business logic."

;; ;; Note that doing so will give us the SIXTH FN. Let me take a stab without looking at his solution:
;; ;; To name a fn, what do you do? You insert a NEW FN, that takes a PARAMETER with the name you want, and you call this new fn, passing
;; ;;  the fn you want to name as an arg.

;; ;; ((fn [mk-length]
;; ;;    (mk-length mk-length))
;; ;;  (fn [mk-length]
;; ;;    ((fn [this-is-the-new-name]
;; ;;       (this-is-the-new-name))
;; ;;     ((fn [length-fn]
;; ;;        (fn [l]
;; ;;          (cond
;; ;;           (null? l) 0
;; ;;           :else (add1 (length-fn (rest l))))))
;; ;;      (fn [x]
;; ;;        ((mk-length mk-length) x))))))

;; ;; This (above) was my first attempt, but it's not right. We don't want to name the fn  that comes from executing the length-fn with
;; ;;  the final fn. We want to actually name the length-fn by itself. So here goes:
;; ((fn [mk-length]
;;    (mk-length mk-length))
;;  (fn [mk-length]
;;    ((fn [this-is-the-new-name length-fn]
;;       (this-is-the-new-name length-fn))
;;     (fn [length-fn]
;;       (fn [l]
;;         (cond
;;          (null? l) 0
;;          :else (add1 (length-fn (rest l))))))
;;     (fn [x]
;;       ((mk-length mk-length) x)))))
;; ;;
;; Awesome! That works, let's see if that's what he had in mind.
;;
;; Not precisely. He did something equivalent, but he made the "business logic" fn be the last thing, and the let-over-lambda that
;;  names it be the first thing. I can see how that could be equivalent. There are a number of ways these nested fns could be
;;  arranged.
;;
;; So, what he winds up with is the "length" fn (aka, the "business logic") being at the end, and completely detangled from the
;;  "infrastructure", and the "infrastructure" we wind up with is the "APPLICATIVE-ORDER Y COMBINATOR"

;; PHEW!

;; Chapter 10

(def new-entry build)

(def lookup-in-entry
  (fn [name entry entry-f]
    (lookup-in-entry-helper name
                            (first entry)
                            (second entry)
                            entry-f)))

(def lookup-in-entry-helper
  (fn [name names values entry-f]
    (cond
     (null? names) (entry-f name)
     (eqan? (first names) name) (first values)
     :else (lookup-in-entry-helper name (rest names) (rest values) entry-f))))

(def is-prime
  (fn [x]
    (fn is-prime-h [x try]
      (cond
       (eqan? x 1) true
       (eqan? x 2) true
       (eqan? 1 try) true
       (zero? (mod x try)) false
       :else (is-prime-h x (sub1 try))
       )) x () ))

(defmacro when-available
  [sym & body]
  (try
    (when (resolve sym)
      (list* 'do body))
    (catch ClassNotFoundException _#)))

(defn- expt-int [base pow]
  (loop [n pow, y (num 1), z base]
    (let [t (even? n), n (quot n 2)]
      (cond
       t (recur n y (mult z z))
       (zero? n) (mult z y)
       :else (recur n (mult z y) (mult z z))))))

(defn expt
  "(expt base pow) is base to the pow power.
Returns an exact number if the base is an exact number and the power is an integer, otherwise returns a double."
  [base pow]
  (if (and (not (float? base)) (integer? pow))
    (cond
     (pos? pow) (expt-int base pow)
     (zero? pow) (cond
                   (= (type base) BigDecimal) 1M
                   (= (type base) java.math.BigInteger) (java.math.BigInteger. "1")
                   (when-available clojure.lang.BigInt (= (type base) clojure.lang.BigInt))
                   (when-available clojure.lang.BigInt (bigint 1))
                   :else 1)
     :else (/ 1 (expt-int base (minus pow))))
    (Math/pow base pow)))

(defn- integer-sqrt [n]
  (cond
   (> n 24)
   (let [n-len (integer-length n)]
     (loop [init-value (if (even? n-len)
			 (expt 2 (quot n-len 2))
			 (expt 2 (inc* (quot n-len 2))))]
       (let [iterated-value (quot (plus init-value (quot n init-value)) 2)]
	 (if (>= iterated-value init-value)
	   init-value
	   (recur iterated-value)))))
   (> n 15) 4
   (> n  8) 3
   (> n  3) 2
   (> n  0) 1
   (> n -1) 0))

(defn- sqrt-integer [n]
  (if (neg? n) Double/NaN
      (let [isqrt (integer-sqrt n),
	    error (minus n (mult isqrt isqrt))]
	(if (zero? error) isqrt
	    (Math/sqrt n)))))
