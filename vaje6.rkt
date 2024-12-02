#lang racket

(define (power x n) ; Correct the define syntax
  (if (= n 0)
      1
      (* x (power x (- n 1))))) ; Fix the recursive call syntax

(define (gcd x y)
(if (= x 0)
    y
    (gcd (remainder y x) x)))

(define (fib n)
(if(< n 2)
   n
   (+ (fib(- n 1)) (fib(- n 2)))))

(define reverse (list ))