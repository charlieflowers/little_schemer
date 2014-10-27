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
     :else (and (charlie-numbered? (first sexp)) (numbered? (rest sexp))))))

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
