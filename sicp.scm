;Exercises from Structure and Interpretation of Computer Programs
(define (myabs x )
  (cond ((>= x 0) x)
        ((< x 0) (- x))))

(define (otherabs x)
  (if (< x 0)
    (- x)
    x))

(define (thirdabs x)
  (cond ((< x 0) (- x))
    (else x )))

;Exercise 1.1 Answers in comments.
10 ;10
(+ 5 3 4) ;12
(- 9 1) ;8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ;6
(define a 3) ; a = 3
(define b (+ a 1)) ; b = 4
(+ a b (* a b)) ;12 + 4 + 3 = 19
(= a b) ; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; 16
(+ 2 (if (> b a) b a)) ; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; 16

;Exercise 1.2 Covert to prefix notation:
;5 + 4 + (2 - (3 - (6 + 4/5))) / 3(6 - 2)(2 - 7)

(define onetwo
  (/
    (+ 5 4
      (- 2
        (- 3
          (+ 6
          (/ 4 5)))))
    (* 3 (- 6 2) (- 2 7))))
; -37/150

;Exercise 1.3 Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.
(define (square_sum x y)
  (+ (square x) (square y)))

(define (larger_sqr_sum x y z)
  (cond ((and (> x z) (> y z)) (square_sum x y))
        ((and (> x y) (> z y)) (square_sum x z))
        ((and (> z x) (> y x)) (square_sum z y))))
;Exercise 1.4 Cool.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; This adds or subtracts b depending on it's sign.

;Exercise 1.5. So I think with normal order (p) will never get evaluated and the infinite recursion trap would never happen, as it does with applicative order.

; 1.1.7

(define (sqrt-iter guess x)
  (define newguess (improve guess x))
  (if (good-enough? guess newguess)
    newguess
    (sqrt-iter newguess x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))


(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqroo x)
  (sqrt-iter 1.0 x))

;Exercise 1.6
;(define (newif predicate then-case else-case)
;  (cond
;    (predicate then-case)
;    (else else-case)))
;;(define (sqrt-iter guess x)
;;  (newif (good-enough? guess x)
;;    guess
;;    (sqrt-iter (improve guess x)
;;                x)))

;Written this way the function recurses infinitely because
;both cases get evaluated before the operator newif is used.
;Thats thanks to applicative evaluation. 'if' must be doing something
;special to lazily evaluate it's arguments. As a function, newif evaluates
;everything.

; Exercise 1.7 Alternate good-enough?
(define (really-good-enough? oldguess newguess)
  (<
    (/
      (abs (- oldguess newguess))
      newguess)
    0.001))

; Exercise 1.8 Cube root
(define (cube-root x)
  (cube-rooter 1.0 x))
(define (cube-rooter guess x)
  (define newguess (cube-improve guess x))
  (if (really-good-enough? guess newguess)
    newguess
    (cube-rooter newguess x)))
(define (cube-improve guess x)
  (/
    (+
      (/ x (square guess))
      (* 2 guess))
    3))

;Refactored cube root
(define (cube-root x)
  (define (cube-rooter guess)
    (define newguess (cube-improve guess))
    (if (really-good-enough? guess newguess)
      newguess
      (cube-rooter newguess)))
  (define (cube-improve guess)
    (/
      (+
        (/ x (square guess))
        (* 2 guess))
      3))
  (define (really-good-enough? oldguess newguess)
  (<
    (/
      (abs (- oldguess newguess))
      newguess)
    0.001))
  (cube-rooter 1.0))

;1.2
(define (factorial num)
  (if (= num 1)
    1
    (* num (factorial (- num 1)))))
(define (other_factorial n)
  (define (factoriter product counter)
    (if (> counter n)
      product
      (factoriter
        (* product counter)
        (+ 1 counter))))
  (factoriter 1 1))

;Exercise 1.9

(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))
(define (plus a b) ; iterative process
  (if (= a 0 )
    b
    (+ (dec a) (inc b))))
(define (plus2 a b) ; recursive process
  (if (= a 0)
    b
    (inc (+ (dec a) b))))
; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* y 2))
        ((= y 1) 2)
        (else (A (- x 1)
                  (A x (- y 1))))))
; (A 1 10) => 1024
; (A 2 4) => 65536
; (A 3 3) => 65536

(define (double n) (A 0 n)) ; n * 2

(define (two_to_the n) (A 1 n)) ; 2 ^ n

(define (power_tower n) (A 2 n)) ; 2 ^ 2 ... n times

(define (k n) (* 5 n n))

(define (fibs n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else
          (+ (fibs (- n 1))
             (fibs (- n 2))))))
(define (fibsiter a b n)
  (if (= 0 n)
    b
    (fibsiter (+ a b) a (- n 1))))
(define (itfibs n)
  (fibsiter 1 0 n))
(define (concat . strings)
  (apply string-append strings))
(define (make-change amount)
  (mc amount 5))
(define (mc amount kinds-of-coins )
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+  (mc amount
                    (- kinds-of-coins 1))
                  (mc (- amount
                        (first-denom kinds-of-coins))
                      kinds-of-coins )))))
(define (first-denom kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
;Exercise 1.11
(define (trip-fibs n) ; Recursive
  (if (< n 3)
    n
    (+
      (trip-fibs (- n 1))
      (* 2 (trip-fibs (- n 2)))
      (* 3 (trip-fibs (- n 3))))))
(define (trifibs-iter a b c n)
  (cond
    ((= n 0) c)
    ((= n 1) b)
    ((= n 2) a)
    (else (trifibs-iter (+ a (* 2 b) (* 3 c)) a b (- n 1)))))
(define (it-trifibs num)
  (trifibs-iter 2 1 0 num))
; Exercise 1.12
(define (pascal-tri row col)
  (if (= 0 row)
    (if (= col 0) 1 0)
    (+
      (pascal-tri (- row 1) (- col 1))
      (pascal-tri (- row 1) col))))
; Exercise 1.13
(define (gold-fibs n)
  (define golden-ratio (/ (+ 1 (sqrt 5)) 2))
  (inexact->exact (round (/ (power golden-ratio n) (sqrt 5)))))

(define (power num exp)
  (exp-iter 1 num exp))
(define (exp-iter product number counter)
  (if (= counter 0)
    product
    (exp-iter (* product number) number (- counter 1))))

