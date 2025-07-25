#lang racket

(define-syntax sml
  (syntax-rules (:: hd tl null nil)
    [(_ x :: xs) (cons x xs)]
    [(_ hd xs) (car xs)]
    [(_ tl xs) (cdr xs)]
    [(_ null xs) (null? xs)]
    [(_ nil) '()]))


(define ones( cons 1 (lambda () ones)))

;(car enke)                    ; prvi element
;(car ((cdr enke)))            ; drugi element
;(car ((cdr ((cdr enke)))))    ; tretji element

(define naturals (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
                  (f 1)))

;(car naturals)

(define fibs
  (letrec([f (lambda(x y)
               (cons x (lambda() (f y (+ x y))))
               )])
(f 1 1)))

;(car((cdr fibs)))
;(car((cdr((cdr fibs)))))

(define (first n tok)
  (if (= 0 n)
      '()
      (cons (car tok)
            (first (- n 1) ((cdr tok))))))

(define (squares tok)
  (cons (* (car tok) (car tok))
        (lambda () (squares ((cdr tok))))))

(define (my-delay think)
  (let ([stored-value #f]
        [call-count 0])
    (lambda ()
      (set! call-count (add1 call-count))
      (if (or (eq? stored-value #f)       
              (= (modulo call-count 5) 1)) 
          (begin
            (set! stored-value (think))
            stored-value)
          stored-value))))

(define (my-force delayed)
  (delayed))

(define (partitions k n)
  (cond
    [(or (< k 1) (< n 1)) 0]
    [(= k n) 1]   
    [(= k 1) 1]      
    [else (+ (partitions k (- n k))
             (partitions (sub1 k) (sub1 n)))]))









(*curry
def curry3(func):
    def curried(*args):
        if len(args) == 3:
            print(f"Curryied parametrov: {len(args)}")
            return func(*args)
        elif len(args) == 2:
            print(f"Curryied parametrov: {len(args)}")
            return lambda c: func(args[0], args[1], c)
        elif len(args) == 1:
            print(f"Curryied parametrov: {len(args)}")
            return lambda b: lambda c: func(args[0], b, c)
        else:
            raise ValueError("Napačno število argumentov")
    return curried

@curry3
def f(a, b, c):
    return a + b + c

# Primeri uporabe
print(f(1, 2, 3))        # Izpiše: Curryied parametrov: 3 \n 6
print(f(1)(2, 3))        # Izpiše: Curryied parametrov: 1 \n Curryied parametrov: 2 \n 6
print(f(1, 2)(3))        # Izpiše: Curryied parametrov: 2 \n Curryied parametrov: 1 \n 6
print(f(1)(2)(3))        # Izpiše: Curryied parametrov: 1 \n Curryied parametrov: 1 \n Curryied parametrov: 1 \n 6



*)