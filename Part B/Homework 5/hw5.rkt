;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out))

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)   #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)      #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions
;; evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; 1-A
;; racket list -> mupl list
;; Takes a Racket list and produces a MUPL list.

(define (racketlist->mupllist r-lst)
  (cond [(empty? r-lst) (aunit)]
        [else
         (apair (car r-lst) (racketlist->mupllist (cdr r-lst)))]))

;; 1-B
;; mupl list -> racket list
;; Takes a MUPL list and produces a Racket list.

(define (mupllist->racketlist m-lst)
  (cond [(aunit? m-lst) '()]
        [else
         (cons (apair-e1 m-lst) (mupllist->racketlist (apair-e2 m-lst)))]))
  
;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function

(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.

(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(int? e) e]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e)
         (let ([v1 (int-num (eval-under-env (ifgreater-e1 e) env))]
               [v2 (int-num (eval-under-env (ifgreater-e2 e) env))])
           (if (> v1 v2)
               (eval-under-env (ifgreater-e3 e) env)
               (eval-under-env (ifgreater-e4 e) env)))]
        [(fun? e) (closure env e)]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
         (if (closure? v1)
             (let* ([clos-env (closure-env   v1)]
                    [fn-v     (closure-fun   v1)]
                    [body     (fun-body    fn-v)]
                    [formal   (fun-formal  fn-v)]
                    [nameopt  (fun-nameopt fn-v)])
               (eval-under-env body (if (equal? nameopt #f)
                                        (cons (cons formal  v2) clos-env)
                                        (cons (cons formal  v2)
                                              (cons (cons nameopt v1) clos-env)))))
             (error "funexp is not a closure")))]
       [(mlet? e)
         (let ([body (mlet-body e)] [v (mlet-var e)]
               [exp (eval-under-env (mlet-e e) env)])
           (eval-under-env body (cons (cons v exp) env)))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v-apair (eval-under-env (fst-e e) env)])
           (apair-e1 v-apair))]
        [(snd? e)
         (let ([v-apair (eval-under-env (snd-e e) env)])
           (apair-e2 v-apair))]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v) (int 1) (int 0)))]
        [(closure? e) e]
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

;; 3-A
;; muplExpression mupleExpression mupleExpression -> Number
;; Takes three mupl expressions e1 , e2, and e3 . It returns a mupl expression that
;; when run evaluates e1 and if the result is mupl’s aunit then it evaluates e2 and
;; that is the overall result, else it evaluates e3 and that is the overall result.

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

;; 3-B
;; racket (listof racket pairs) muplExpression -> muplExpression
;; Takes a Racket list of Racket pairs ’((s1 . e1) . . . (si . ei) . . . (sn . en))
;; and a final mupl expression en+1 . In each pair, assume si is a Racket string and
;; ei is a mupl expression. mlet* returns a mupl expression whose value is en+1
;; evaluated in an environment where each si is a variable bound to the result of
;; evaluating the corresponding ei for 1 ≤ i ≤ n. The bindings are done sequentially,
;; so that each ei is evaluated in an environmen where s1 through si−1 have been
;; previously bound to the values e1 through ei−1 .

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

;; 3-C
;; muplExpression muplExpression muplExpression muplExpression -> muplExpression
;; Takes four mupl expressions e1 , e2 , e3, and e4 and returns a mupl expression
;; that acts like ifgreater except e3 is evaluated if and only if e1 and e2 are
;; equal integers.

(define (ifeq e1 e2 e3 e4)
  (ifgreater e1 e2 (ifgreater e2 e1 e3 e4) (ifgreater e2 e1 e4 e3)))

;; Problem 4

;; 4-A
;; mupl function -> mupl function -> mupl list -> mupl list
;; A curried function, that takes a mupl function and return a mupl function that
;; takes a mupl list and applies the function to every element of the list returning
;; a new mupl list.

(define mupl-map
  (fun #f "f" (fun "m-function" "m-list"
                   (ifaunit (var "m-list")
                            (aunit)
                            (apair (call (var "f") (fst (var "m-list")))
                                   (call (var "m-function") (snd (var "m-list"))))))))

;; 4-B
;; Racket variable that takes an mupl integer i and returns a mupl function that
;; takes a mupl list of mupl integers and returns a new mupl list of mupl integers
;; that adds i to every element of the list. 

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i" (call (var "map")
                          (fun #f "m-list" (add (var "i") (var "m-list")))))))

;(notice map is now in MUPL scope)))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent)
;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))