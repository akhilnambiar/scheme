;;; Test Cases for the Scheme Project 

;; To run all tests:
;;     python3 scheme_test.py tests.scm
;;


;; The following should work for the initial files.

3
; expect 3

-123
; expect -123

1.25
; expect 1.25

#t
; expect #t

#f
; expect #f

)
; expect Error

;; In the following sections, you should provide test cases, so that by 
;; running 
;;     python3 scheme_test.py tests.scm
;; you can test the portions of the project you've completed.  In fact, 
;; you might consider writing these tests BEFORE tackling the associated
;; problem!

; Problem 1  (the reader)
;   Initially, the project skeleton simply reads and prints the expressions
;   entered.  Later on, it starts evaluating them.  You may therefore need
;   to modify the tests you initially provide for this section when you get
;   to later sections.  For example, initially, entering the symbol x will
;   simply cause the evaluator to print "x" rather than attempting to evaluate
;   it (and getting an error).  Therefore, you may later have to modify
;   x to (e.g.) 'x

; YOUR TEST CASES HERE
'(7 11)
; expect (7 11)

'(5 4 (3 . 2) 1)
; expect (5 4 (3 . 2) 1)


; Problem A2 and B2 (symbol evaluation and simple defines)

; YOUR TEST CASES HERE
(define a 9)
x
; expect 9

(define b 8473)
b
; expect 8473

(define c 1)
c
; expect 1


; Problem 3 (primitive function calls)

; YOUR TEST CASES HERE
(+ 7 11)
;expect 18

(define x 4)
(define y 3)
(+ x y)
;expect 7



; Problem A4, B4, A5, B5, and 6 (calls on user-defined functions)

; YOUR TEST CASES HERE
(lambda (x) (+ x x))
; expect UNSPEC
(lambda (a, b) (* a b))
; expect UNSPEC

(define (a x) (+ x 3))
;expect UNSPEC

(a 3)
;expect 6

(define (f x y) (* x y) )
;expect UNSPEC

(f 9 5)
;expect 45

(define a 4)
;expect UNSPEC

(define (f a)
(define a 9)
(+ a 1))
;expect UNSPEC
a
;expect 4
(f 3)
;expect 4


; Problem 7 (set!)

; YOUR TEST CASES HERE
(define a 1)
(define (test) (set! x 2) x)
; expect 2
(define x 1)
(define (test) (set! x (/ 1 0)) x)
;expect Error
(define x 2)
(define (test) (set! x 3) x)
;expect 



; Problem A8 (if, and)

; YOUR TEST CASES HERE
(if (eqv? 1 1) 3 4 5)
; expect Error
(if (eqv? 1 1) 5)
; expect Error
(if (eqv? 1 2) 1 2)
; expect 2
(if (eqv? 6 6) 9 0)
; expect 9



(and #f #f)
; expect #f
(and #t #f)
; expect #f
(and #t #t)
; expect #t
(and #f (/ 1 0))
; expect #f
(and #t #t #f #t)
; expect #f


; Problem B8 (cond, or)

; YOUR TEST CASES HERE
(cond ((eqv? 2 3) 1) ((eqv? 3 3) 2) (else 3))
; expect 2
(cond ((eqv? 3 3) 1) ((eqv? (/ 1 0) 3) #t) (else 3))
; expect 1
(cond ((eqv? 3 3) 1) ((eqv? 2 3) (/ 1 0)) (else 2))
; expect 1

(or #t #f)
; expect #t
(or #f #f)
; expect #f
(or #f #t)
; expect #t


; Problem 9 (let)

; YOUR TEST CASES HERE
(let ((x 5) (y 3))
      (let ((x 8) (z (+ x y)))
      (* z x)))
;expect 64


; Extra Credit 1 (let*)

; YOUR TEST CASES HERE
(let* ((x 7) (y (+ x 3))) y)
;expect 10



; Extra Credit 2 (case)

; YOUR TEST CASES HERE
(case (* 1 3)
  ((2 3 5 7) 'prime)
  ((1 4 27 100) 'composite))
;expect 'prime

(case (car '(c d))
  ((a e i o u) 'vowel)
  ((w y) 'semivowel)
  (else 'consonant))
;expect 'consonant

(case (* 1 2)
  (a b 'green)
  ((1 4 6 8 9) 'blue))
;expect Error


; Problem A10

;; The subsequence of list S for which F outputs a true value (i.e., one
;; other than #f), computed destructively
(define (filter! f s)
   ; *** YOUR CODE HERE ***
(define (filter! pred lst)
    (if (null? lst)
         lst
    (if (pred (car lst))
        (begin (set-cdr! lst (filter! pred (cdr lst))) lst)
        (let ((temp (filter! pred (cdr lst)))) temp)
        )
    )
)

(define (big x) (> x 5))

(define ints (list 1 10 3 8 4 7))
(define ints1 (cdr ints))

(define filtered-ints (filter! big ints))
filtered-ints
; expect (10 8 7)
(eq? filtered-ints ints1)
; expect #t


; Problem A11.

;; The number of ways to change TOTAL with DENOMS
;; At most MAX-COINS total coins can be used.
(define (count-change total denoms max-coins)

    (cond ((< max-coins 0) 0)
              ((equal? total 0) 1)
	  ((null? denoms) 0)
	  ((>= total (car denoms)) (+ (count-change total (cdr denoms) max-coins) 
                              (count-change (- total (car denoms)) denoms (- max-coins 1))))
	   (else (count-change total (cdr denoms) max-coins))))

(define us-coins '(50 25 10 5 1))
(count-change 20 us-coins 18)
; expect 8

; Problem B10

;; Reverse list L destructively, creating no new pairs.  May modify the 
;; cdrs of the items in list L.
(define (reverse! L)
   ; *** YOUR CODE HERE ***
(define (reverse reversed seq)

    (if (null? seq)
        reversed
     (let ((temp (cdr seq)))
         (set-cdr! seq reversed) 
         (reverse seq temp)
             )
         )
     )
(reverse '() L)
)

(define L (list 1 2 3 4))
(define LR (reverse! L))
LR
; expect (4 3 2 1)

(eq? L (list-tail LR 3))
; expect #t

; Problem B11

;; The number of ways to partition TOTAL, where 
;; each partition must be at most MAX-VALUE
(define (count-partitions total max-value)
  ; *** YOUR CODE HERE ***
  (define (gen-numbers total)
        (if (= 0 total)
            '()
            (cons total (gen-numbers (- total 1)))
        )
     )

    (define (partitions amount denoms)
        (cond ((= amount 0) 1)
       ((null? denoms)
       0
       )
       ((>= amount (car denoms))
                            (+ (partitions amount (cdr denoms))
                                 (partitions (- amount (car denoms)) denoms)
                            )
       )
      (else
              (partitions amount (cdr denoms))
      )))

(partitions total (gen-numbers max-value) )
)

(count-partitions 5 3)
; expect 5
; Note: The 5 partitions are [[3 2] [3 1 1] [2 2 1] [2 1 1 1] [1 1 1 1 1]]

; Problem 12

;; A list of all ways to partition TOTAL, where  each partition must 
;; be at most MAX-VALUE and there are at most MAX-PIECES partitions.
(define (list-partitions total max-pieces max-value)
; *** YOUR CODE HERE ***
(define (partitions l total mp max)
  (cond ((= total 0) (list l))
        ((or (< total 0) (<= max 0) (= mp 0)) '())
        (else (append (partitions (append l (list max)) (- total max) (- mp 1) max)
                      (partitions l total mp (- max 1))))))
(partitions '() total max-pieces max-value))

(list-partitions 5 2 4)
; expect ((4 1) (3 2))
(list-partitions 7 3 5)
; expect ((5 2) (5 1 1) (4 3) (4 2 1) (3 3 1) (3 2 2))



