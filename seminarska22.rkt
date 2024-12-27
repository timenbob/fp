#lang racket

(struct true () #:transparent)
(struct false () #:transparent)
(struct int (n) #:transparent)
(struct empty () #:transparent)
(struct .. (e1 e2) #:transparent)
(struct exception (exn) #:transparent)
(struct triggered (e) #:transparent)

(define (fri e okolje)
  (cond
    [(triggered? e) e] ; Če je že sprožena izjema
    [(exception? e) (triggered e)] ; Če je izrazu tipa izjema
    [(true? e) e] ; Če je logična vrednost (true)
    [(false? e) e] ; Če je logična vrednost (false)
    [(int? e) e] ; Če je celo število
    [(empty? e) e] ; Če je prazno zaporedje
    [(..? e) 
     (let ([first (fri (..-e1 e) okolje)]
           [rest (fri (..-e2 e) okolje)])
       (if (triggered? first)
           first
           (if (triggered? rest)
               rest
               (.. first rest))))]
    [else (error "Neznan izraz" e)]))

(define (trigger e)
  (let ([result (fri e '())]) ; Evalviramo izraz
    (if (exception? result)
        (triggered result)
        (triggered (exception "trigger: wrong argument type")))))

(define (triggered? x) (triggered? x))
(define (exception? x) (struct? x exception))
(define (true? x) (struct? x true))
(define (false? x) (struct? x false))
(define (int? x) (struct? x int))
(define (empty? x) (struct? x empty))
(define (..? x) (struct? x ..))

(define (handle e1 e2 e3 environment)
  (let ([val1 (fri e1 environment)]
        [val2 (fri e2 environment)])
    (cond
      ;; Rule 1: e1 does not evaluate to a triggered exception
      [(not (triggered? val1))
       (trigger (exception "handle: wrong argument type"))]

      ;; Rule 2: e2 evaluates to a triggered exception that matches e1
      [(triggered? val2)
       (if (exception-matches? (get-exception val1) (get-exception val2))
           (fri e3 environment)
           val2)]

      ;; Rule 3: No exceptions
      [else val2])))

(define (if-then-else condition e1 e2 environment)
  (let ([cond-result (fri condition environment)])
    (if (equal? cond-result (false))
        (fri e2 environment) ; Evaluate and return `e2` if condition is `false`
        (fri e1 environment)))) ; Otherwise, evaluate and return `e1`

(define (?int e environment)
  (let ([result (fri e environment)])
    (if (struct? result 'int)
        (true)
        (false))))

(define (?bool e environment)
  (let ([result (fri e environment)])
    (if (or (equal? result (true)) (equal? result (false)))
        (true)
        (false))))

(define (?.. e environment)
  (let ([result (fri e environment)])
    (if (or (struct? result '..) (equal? result (empty)))
        (true)
        (false))))

(define (?seq e environment)
  (let ([result (fri e environment)])
    (if (and (struct? result '..)
             (equal? (fri (..-e2 result) environment) (empty)))
        (true)
        (false))))

(define (?empty e environment)
  (let ([result (fri e environment)])
    (if (equal? result (empty))
        (true)
        (false))))

(define (?exception e environment)
  (let ([result (fri e environment)])
    (if (struct? result 'exception)
        (true)
        (false))))








































