#lang racket

(define (power x y) (
    if (= y 1)
       x
       (* x (power x (- y 1)))))

(define odg (power 2 3))



(define (reverse sez )
(define (pomoc sez acc)
 (if (null? sez) acc
     (pomoc (cdr sez) (cons (car sez) acc))
  ))
(pomoc sez null))
                    

(define odg2 (reverse (list 1 2 3)))




(define (range z k korak)
  (define (pomoc z k korak acc)
  (if (>= (+ z korak) k)
      acc
      (pomoc (+ z korak) k korak (cons (+ z korak) acc))))
  (reverse (pomoc z k korak (list z))))


(define gg3 (range 1 9 3))
(write gg3)



(define (naturals)
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
                  (f 1)))

(define (fib)
  (letrec ([ f(lambda (x y) (cons x (lambda() (f y (+ x y)))))])
  (f 1 1)))


(define (potencna s)
  (if (null? s)
      '(())  ; Potenčna množica prazne množice je množica, ki vsebuje samo prazno množico
      (let* ([ostalo (potencna (cdr s))]  ; Rekurzivno kličemo za preostanek seznama
             [z-novim (map (lambda (x) (cons (car s) x)) ostalo)]) ; Dodamo prvi element v vse podmnožice
        (append ostalo z-novim))))  ; Združimo obstoječe in nove podmnožice


      