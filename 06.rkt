#lang racket

(define (power x n)
  (if (= n 0)
      1
      (* x (power x (- n 1))))) 

(define (gcd x y)
(if (= x 0)
    y
    (gcd (remainder y x) x)))

(define (fib n)
(if(< n 2)
   n
   (+ (fib(- n 1)) (fib(- n 2)))))

(define (reverse sez )
(define (pomoc sez acc)
 (if (null? sez) acc
     (pomoc (cdr sez) (append (car sez) acc))
  ))
(pomoc sez null))


(define (remove x sez)
  (define (pomoc sez acc x)
 (if (null? sez) (reverse acc)
     (if (= (cdr sez) x)
       (pomoc (cdr sez) acc)
      (pomoc (cdr sez) (cons (car sez) acc) x)))
  )
(pomoc sez null x))


(define (map f sez)
  (define (pomoc sez acc)
 (if (null? sez) (reverse acc)
      (pomoc (cdr sez) (cons (f(car sez)) acc))))
  
(pomoc sez null))


(define (filter f sez)
  (define (pomoc sez acc f)
 (if (null? sez) acc
     (if (f (cdr sez))
        (pomoc (cdr sez) (cons (car sez) acc) f)
       (pomoc (cdr sez) acc f)
      ))
  )
(pomoc sez null f))


(define (zip sez1 sez2)
  (define (pomoc sez1 sez2 acc)
 (if (null? sez1) acc
        (pomoc (cdr sez1) (cdr sez2) (cons (cons (car sez1) (car sez2)) acc))))
      
(pomoc sez1 sez2 null ))

(define (range start end step)
  (define (pomoc current acc)
    (if (> current end)
        (reverse acc)   
        (pomoc (+ current step) (cons current acc))))  
  (pomoc start null))






