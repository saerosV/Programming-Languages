#lang racket
;; Programming Languages Homework 4 Tests

(require "hw4.rkt")

(require rackunit)

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define a 2)

(define tests
  (test-suite
   "Sample tests for Assignment 4"
   
   ; sequence test
   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5)  "Sequence test")
   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5)  "Sequence test")
   (check-equal? (sequence 1 10 2) (list 1 3 5 7 9)   "Sequence test")
   (check-equal? (sequence 0 16 4) (list 0 4 8 12 16) "Sequence test")

   ; string-append-map test
   (check-equal? (string-append-map (list "you're" "they're" "I'm") " cool")
                 '("you're cool" "they're cool" "I'm cool") "string-append-map test")
   (check-equal? (string-append-map (list "dan" "dog" "curry" "dog2") ".jpg")
                 '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test")
   
   ; list-nth-mod test
   (check-equal? (list-nth-mod (list 0 1 "a" 3 4) 2) "a" "list-nth-mod test")
   (check-equal? (list-nth-mod (list 0 1 2 "b" 4) 3) "b" "list-nth-mod test")
   (check-equal? (list-nth-mod (list 0 1 2 3 "c") 4) "c" "list-nth-mod test")
   (check-equal? (list-nth-mod (list "d" 1 2 3 4) 5) "d" "list-nth-mod test")
   
   ; stream-for-n-steps test
   (check-equal? (stream-for-n-steps ones 2) (list 1 1) "stream-for-n-steps test")
   (check-equal? (stream-for-n-steps ones 6) (list 1 1 1 1 1 1) "stream-for-n-steps test")
   
   ; funny-number-stream test
   (check-equal? (stream-for-n-steps funny-number-stream 16)
                 (list 1 2 3 4 -5 6 7 8
                       9 -10 11 12 13 14 -15 16) "funny-number-stream test")
   (check-equal? (stream-for-n-steps funny-number-stream 25)
                 (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13
                       14 -15 16 17 18 19 -20 21 22 23 24 -25) "funny-number-stream test")
   
   ; dan-then-dog test
   (check-equal? (stream-for-n-steps dan-then-dog 1) (list "dan.jpg") "dan-then-dog test")
   (check-equal? (stream-for-n-steps dan-then-dog 6)
                 (list "dan.jpg" "dog.jpg" "dan.jpg"
                       "dog.jpg" "dan.jpg" "dog.jpg") "dan-then-dog test")
   
   ; stream-add-zero test
   (check-equal? (stream-for-n-steps (stream-add-zero dan-then-dog) 0)
                 '() "stream-add-zero test")
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 1)
                 (list (cons 0 1)) "stream-add-zero test")
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 2)
                 (list (cons 0 1) (cons 0 1)) "stream-add-zero test")
      (check-equal? (stream-for-n-steps (stream-add-zero dan-then-dog) 1)
                 (list (cons 0 "dan.jpg")) "stream-add-zero test")
   (check-equal? (stream-for-n-steps (stream-add-zero dan-then-dog) 2)
                 (list (cons 0 "dan.jpg") (cons 0 "dog.jpg")) "stream-add-zero test")
   
   ; cycle-lists test
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3)
                 (list (cons 1 "a") (cons 2 "b") (cons 3 "a")) "cycle-lists test")
   
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 7)
                 '((1 . "a") (2 . "b") (3 . "a") (1 . "b")
                             (2 . "a") (3 . "b") (1 . "a")) "cycle-lists test")
   (check-equal? (stream-for-n-steps (cycle-lists (list 21 33 77) (list "x" "y" "z")) 6)
                 '((21 . "x") (33 . "y") (77 . "z")
                   (21 . "x") (33 . "y") (77 . "z")) "cycle-lists test")
   
   ; vector-assoc test
   (check-equal? (vector-assoc 9 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1)))
              #f "vector-assoc test")
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1)))
                 (cons 4 1) "vector-assoc test")
   (check-equal? (vector-assoc 77 (vector (cons 66 1) (cons 77 2) "a" (cons 88 3) "b"))
                 (cons 77 2) "vector-assoc test")
   
   
   
   ; cached-assoc tests
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 4)
                 #f "cached-assoc test")
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3)
                 (cons 3 4) "cached-assoc test")
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 2) 3)
                 (cons 3 4) "cached-assoc test vec of 2")
  
   
   ; while-less test
   ;(check-equal? (while-less 7 do (begin (set! a (+ a 1)) a)) #t "while-less test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
