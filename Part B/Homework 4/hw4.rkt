#lang racket

(require rackunit)
(provide (all-defined-out))

;; Put your code below

;; 1.
;; Int Int Int -> (listOf Number)
;; Takes 3 arguments low, high, and stride. ASSUME: all arguments are numbers,
;; stride is positive, sequence produces a list of numbers from low to high
;; (including low and possibly high) separated by stride and in sorted order.

(define (sequence low high stride)
  (define (helper acc)
    (cond [(> acc high) null]
          [#t (cons acc (helper (+ acc stride)))]))
  (helper low))

;; 2.
;; (listOf String) String -> (listOf String)
;; Takes a list of strings xs and a string suffix and returns a list of strings.
;; Each element of the output should be the corresponding element of the input
;; appended with suffix (with no extra space between the element and suffix). 

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; 3.
;; (listOf X) Number -> X
;; Takes a list xs and a number n. If the number is negative,terminate the
;; computation with (error "list-nth-mod: negative number"). Else if the list
;; is empty, terminate the computation with (error "list-nth-mod: empty list").
;; Else return the ith element of the list where we count from zero and i is
;; the remainder produced when dividing n by the list’s length. 

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; 4.
;; Stream Number -> (listOf X)
;; takes a stream s and a number n. It returns a list holding the first n values
;; produced by s in order. ASSUME: n is non-negative.

(define (stream-for-n-steps stream number)
  (letrec ([f (lambda (stream n)
                (let ([pr (stream)])
                (cond [(= n 0) null]
                      [#t (cons (car pr) (f (cdr pr) (- n 1)))])))])
    (f stream number)))

;; 5.
;; Stream
;; Similar to a stream of natural numbers (i.e., 1, 2, 3, ...) except numbers
;; divisble by 5 are negated (i.e., 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...). 

(define funny-number-stream
  (letrec ([f (lambda (n)
                (if (= (remainder n 5) 0)
                    (cons (* n -1) (lambda () (f (+ (abs n) 1))))
                    (cons n (lambda () (f (+ n 1))))))])
    (lambda () (f 1))))

;; 6.
;; Stream
;; The elements of the stream alternate between the strings "dan.jpg" and "dog.jpg"
;; (starting with "dan.jpg"). More specifically, dan-then-dog should be a thunk that
;; when called produces a pair of "dan.jpg" and a thunk that when called produces a
;; pair of "dog.jpg" and a thunk that when called... etc.

;; Mutual-recursive solution:

(define (dan-then-dog)
  (define fn1 (lambda () (cons "dan.jpg" fn2)))
  (define fn2 (lambda () (cons "dog.jpg" fn1)))
  (fn1))

;; Alternative way to solve the problem:

;(define dan-then-dog (lambda ()
;                       (cons "dan.jpg" (lambda () (cons "dog.jpg" dan-then-dog)))))

;; 7.
;; Stream -> Stream
;; Takes a stream s and returns another stream. If s would produce v for its
;; ith element, then (stream-add-zero s) would produce the pair (0 . v) for
;; its ith element. 

(define stream-add-zero
  (lambda (stream)
    (let ([s (stream)])
      (lambda () (cons (cons 0 (car s)) (stream-add-zero (cdr s)))))))

;; List -> Stream
;; Key auxiliary function for the first version of cycle-lists.
;; Takes a list and produces a stream.

(define (list-to-stream lst)
    (letrec ([f (lambda (xs)
                    (cond [(null? xs) (f lst)]
                          [#t (cons (car xs) (lambda () (f (cdr xs))))]))])
      (lambda () (f lst))))

;; 8.
;; List List -> Stream
;; Takes two lists xs and ys and returns a stream.  The elements produced by the
;; stream are pairs where the first part is from xs and the second part is from ys.
;; The stream cycles forever through the lists.

;; First version, relies on the output of list-to-stream.

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x y)
                (let ([s1 (x)] [s2 (y)])
                  (cons (cons (car s1) (car s2))
                        (lambda () (f (cdr s1) (cdr s2))))))])
    (lambda () (f (list-to-stream xs) (list-to-stream ys)))))

;; Second version, relies on the output of list-nth-mod, as proposed in the assignment.

;  (define (cycle-lists xs ys)
;      (letrec ([f (lambda (n)
;                    (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
;                          (lambda () (f (+ n 1)))))])
;        (lambda () (f 0))))

;; 9.
;; Value Vector -> Pair
;; Takes a value v and a vector vec. It behaves like Racket’s assoc library function
;; except it processes a vector instead of a list, it allows vector elements not to
;; be pairs in which case it skips them, and it always takes exactly two arguments.
;; Processes the vector elements in order starting from 0. 

(define (vector-assoc v vec)
  (letrec ([helper (lambda (slot-pos only-pair-vec)
                     (if (= slot-pos (vector-length only-pair-vec))
                         #f
                         (let ([slot (vector-ref only-pair-vec slot-pos)])
                           (cond [(equal? v (car slot)) slot]
                                 [#t (helper (+ slot-pos 1) only-pair-vec)]))))])
    (helper 0 (vector-filter pair? vec))))

;; 10.
;; List Number -> Value -> Pair
;; Takes a list xs and a number n and returns a function that takes one argument v
;; and returns the same thing that (assoc v xs) would return. However, it uses an
;; n-element cache of recent results to make this function faster than just calling
;; assoc (if xs is long and a few elements are returned often).
;;
;; The cache is a vector of length n that is created by the call to cached-assoc and
;; used (and possibly mutated) each time the function returned by cached-assoc is
;; called. ASSUME: n is positive.

;; How to call the function:

; ((cached-assoc (listof X) n) v)

(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)] [slot-pos 0])
        (lambda (v)
           (let ([vec-ans (vector-assoc v cache)]
                 [assoc-ans (assoc v xs)])
             (cond [(pair? vec-ans) vec-ans]
                   [(pair? assoc-ans)
                    (begin (vector-set! cache slot-pos assoc-ans)
                           (set! slot-pos (if (= (+ slot-pos 1) n)
                                              0
                                              (+ slot-pos 1)))
                           assoc-ans)]
                   [else #f])))))