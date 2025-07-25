#lang racket

; med seminarsko sem si pomagal s kolegi in umetno inteligenco

(provide false true int .. empty exception
         trigger triggered handle
         if-then-else
         ?int ?bool ?.. ?seq ?empty ?exception
         add mul ?leq ?= head tail ~ ?all ?any
         vars valof fun proc closure call
         greater rev binary filtering folding mapping
         fri)

;; FR Interpretor

;; def stvari
(struct true ()#:transparent)
(struct false ()#:transparent)
(struct int (n)#:transparent)
(struct .. (e1 e2)#:transparent)
(struct empty ()#:transparent)
(struct exception (e)#:transparent)
(struct triggered (e)#:transparent)
(struct trigger (e)#:transparent)
(struct handle (e1 e2 e3)#:transparent)
(struct if-then-else (cond e1 e2) #:transparent)
(struct add (e1 e2) #:transparent )
(struct mul (e1 e2) #:transparent )

(struct ?int (e1)#:transparent)
(struct ?bool (e1)#:transparent)
(struct ?.. (e1)#:transparent)
(struct ?seq (e1)#:transparent)
(struct ?empty (e1)#:transparent)
(struct ?exception (e1)#:transparent)
(struct ?leq (e1 e2)#:transparent)
(struct ?= (e1 e2)#:transparent)

(struct head (e1)#:transparent)
(struct tail (e1)#:transparent)

(struct ~ (e1)#:transparent)
(struct ?all (e1)#:transparent)
(struct ?any (e1)#:transparent)

(struct vars (s e1 e2)#:transparent)
(struct valof (s)#:transparent)

(struct fun (name args body)#:transparent)        ;; Funkcija
(struct proc (name body)#:transparent)            ;; Procedura
(struct closure (env fun)#:transparent)           ;; Funkcijska ovojnica
(struct call (e1 e2)#:transparent)

;; FR Interpretor
(define (fri ex environment)
  (cond; uporabil sem cond nasvet od kolega
    [(triggered? ex) ex] ;If (triggered? e) evaluates to #t (true), the function returns e.
    [(exception? ex) ex]
    [(true? ex) ex]
    [(false? ex) ex]
    [(int? ex) ex]
    [(empty? ex) ex]
    
    [(..? ex) 
     (let ([v1 (fri (..-e1 ex) environment)]
           [v2 (fri (..-e2 ex) environment)])
       (cond
         [(triggered? v1) v1]
         [(triggered? v2) v2]
         [else (.. v1 v2)]))]
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    [(trigger? ex); ce je true naredi to
     (let ([odg (fri (trigger-e ex) environment)]); iz ex ka je trigger vn potegnemo element oznacen z e
       (cond
         [(and (not (exception? odg)) (not (triggered? odg))) (triggered (exception "trigger: wrong argument type"))] ;ce odg ni nic pol napak
         
         [(triggered? odg) odg]
         
         [else (triggered odg)] ;triggered odg
         ))]
    
    [(handle? ex) 
     (let ([r1 (fri (handle-e1 ex) environment)]  ; iz ex potegnemo vn el oznacen z e1
           [r2 (fri (handle-e2 ex) environment)]  
           [r3 (fri (handle-e3 ex) environment)]) 
       (cond
         [(triggered? r1) r1] ;ce je sprozena izjema samo pejdi 
         [(not (exception? r1)) (triggered (exception "handle: wrong argument type"))] ;ce ni izjema pol napaka
         [(triggered? r2) ;ce je sprozena izjema
          (let ([exn2 (triggered-e r2)]) ;extractamo exception iz triggered
            (if (equal? (exception-e exn2) (exception-e r1)) ;ali sta exceptiona iz r1 in r2 enaka
                r3 ;vnremo evalviran r3
                r2))] ;sicer r2
         [else r2] ;ce r2 ne sprozi izjeme evalviran izraz r2
         ))]

;;;;;;;;;;;;;;
    
     [(if-then-else? ex)
       (match (fri (if-then-else-cond ex) environment);potegnemo vn cond
      
         [(false) (fri (if-then-else-e2 ex) environment)] ;cond=false
         [_ (fri (if-then-else-e1 ex) environment)])]   ; sicer


     ;;;;;;;;;;;;;;;;;;
    [(?int? ex) 
     (let ([v (fri (?int-e1 ex) environment)]);;?int-e: Extracts or evaluates the integer-related component of e.
       (cond
         [(and (int? v) (integer? (int-n v))) (true)]
         [(triggered? v) v]
         [else (false)]))]
    
    [(?bool? ex)
     (let ([result (fri (?bool-e1 ex) environment)])
       (cond
         [(or (equal? result (true)) (equal? result (false))) (true)]
         [(triggered? result) result]
         [else (false)]))]
    
    [(?..? ex)
     (let ([v (fri (?..-e1 ex) environment)])
       (cond
         [(empty? v) (false)]
         [(..? v) (true)]
         [else (false)]))]
    
    [(?seq? ex)
     (let loop ([result (fri (?seq-e1 ex) environment)])
       (cond
         [(empty? result) (true)]
         [(..? result)
          (let ([v1 (fri (..-e1 result) environment)]
                [v2 (fri (..-e2 result) environment)])
               (cond
                 [(empty? v2) (true)]
                 [(..? v2) (loop v2)]
                 [else (false)]))]
         [else (false)]))]
    
    [(?empty? ex)
     (let ([result (fri (?empty-e1 ex) environment)])
       (cond
         [(empty? result) (true)]
         [(triggered? result) result]
         [else (false)]))]
    
    [(?exception? ex)
     (let ([result (fri (?exception-e1 ex) environment)])
       (if (struct? result 'exception)
        (true)
        (false)))]
    ;;;;;;;;;;;;;;;;;;;;;;;;;;
    [(add? ex)
     (match* ((fri (add-e1 ex) environment) (fri (add-e2 ex) environment)); match* pogleda kombinacije prvi pa drugi el el
       [((int v1) (int v2)) (int (+ v1 v2))]
       [((false) (false)) (false)]
       [((true) (true)) (true)]
       [((false) (true)) (true)]
       [((true) (false)) (true)]
       [((empty) (empty)) (empty)]

       
       [((triggered e1) _) (triggered e1)]
       [(_ (triggered e2)) (triggered e2)]
       
       [((empty) (.. v2 v2-rest)) (.. v2 v2-rest)]
       [((.. v1 v1-rest) (empty)) (.. v1 v1-rest)]
       
       [((.. v1 v1-rest) (.. v2 v2-rest))
        (let ([tail (fri (add v1-rest (.. v2 v2-rest)) environment)])
          (cond
            [(empty? v1-rest) (.. v1 (.. v2 v2-rest))]
            [(triggered? tail) tail]
            [else (.. v1 tail)]))]
       [(_ _) (triggered (exception "add: wrong argument type"))])];; ce sta oba prazna

    
    [(mul? ex);; +- zgornja ideja dall sem da zgeneriria chat po kodi za add in naredil popravke
     (match* ((fri (mul-e1 ex) environment) (fri (mul-e2 ex) environment))
       [((int v1) (int v2)) (int (* v1 v2))]
       [((true) (true)) (true)]
       [((false) (false)) (false)]
       [((false) (true)) (false)]
       [((true) (false)) (false)]
       [((triggered e1) _) (triggered e1)]
       [(_ (triggered e2)) (triggered e2)]
       [(_ _) (triggered (exception "mul: wrong argument type"))])]
    
    [(?leq? ex)
     (match* ((fri (?leq-e1 ex) environment) (fri (?leq-e2 ex) environment))
       [((int v1) (int v2)) (if (<= v1 v2) (true) (false))]
       [((true) (false)) (false)]
       [((false) (true)) (true)]
       [((true) (true)) (true)]
       [((false) (false)) (true)]
       [((empty) (empty)) (true)]
       [((.. e1-rest e1-next) (empty)) (false)]
       [((empty) (.. e2-rest e2-next)) (true)]
       [((.. e1-rest e1-next) (.. e2-rest e2-next)) (if (<= (length e1-next) (length e2-next)) (true) (false))]
       [((triggered e1) _) (triggered e1)]
       [(_ (triggered e2)) (triggered e2)]
       [(_ _) (triggered (exception "?leq: wrong argument type"))])]
    
    [(?=? ex)
     (match* ((fri (?=-e1 ex) environment) (fri (?=-e2 ex) environment))
       [((int v1) (int v2)) (if (= v1 v2) (true) (false))]
       [((true) (true)) (true)]
       [((false) (false)) (false)]
       [((true) (false)) (false)]
       [((false) (true)) (false)]
       [((.. e1 n1) (.. e2 n2)) (if (and (equal? e1 e2) (equal? n1 n2)) (true) (false))]
       [(_ _) (false)])]

    ;rep glava part
    
    [(head? ex)
     (let ([v (fri (head-e1 ex) environment)])
       (cond
         [(empty? v) (triggered (exception "head: empty sequence"))]
         [(not (..? v)) (triggered (exception "head: wrong argument type"))]
         [else (..-e1 v)]))]; vrni glavo
    
    [(tail? ex)
     (let ([v (fri (tail-e1 ex) environment)])
       (cond
         [(empty? v) (triggered (exception "tail: empty sequence"))]
         [(not (..? v)) (triggered (exception "tail: wrong argument type"))]
         [else (..-e2 v)]))]

    
    [(~? ex)
     (let ([v (fri (~-e1 ex) environment)])
       (cond
         [(true? v) (false)]
         [(false? v) (true)]
         [(int? v) (int (- (int-n v)))]
         [else (triggered (exception "~: wrong argument type"))]))]
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
    [(?all? ex)
     (match* ((fri (?all-e1 ex) environment))
                     [((empty)) (true)]
                     [((.. v1 v1-rest))
                      (let* ([vv1 (fri v1 environment)]  
                             [vv1-rest (fri (?all v1-rest) environment)])  
                        (cond
                          [(triggered? vv1) vv1]  
                          [(triggered? vv1-rest) vv1-rest]
                          
                          [(false? vv1) (false)] 
                          [(false? vv1-rest) (false)]
                          
                          [(and (true? vv1) (true? vv1-rest)) (true)]
                          [(and (empty? vv1) (empty? vv1-rest)) (true)]
                          [(and (true? vv1) (empty? vv1-rest)) (true)]
                          
                          [(empty? v1) vv1-rest]
                          
                          [else vv1-rest]))]
                     [(_) (triggered (exception "?all: wrong argument type"))])]
    
    [(?any? ex)
     (match* ((fri (?any-e1 ex) environment))
                     [((empty)) (false)]; prazen naredi to
       
                     [((.. v1 v1-rest)); ce uspes neki vn potegnt je bistu seznam
                      (let* ([vv1 (fri v1 environment)]  
                             [vv1-rest (fri (?any v1-rest) environment)])  
                        (cond
                          [(triggered? vv1) vv1]  
                          [(triggered? vv1-rest) vv1-rest]  
                          [(true? vv1) (true)] 
                          [(true? vv1-rest) (true)]
                          [(and (empty? vv1) (empty? vv1-rest)) (false)] 
                          [else vv1-rest]))]
       
                     [(_) (triggered (exception "?any: wrong argument type"))])];ni ga naredi to

    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     [(vars? ex)
      (let* ([s (vars-s ex)];;;;Vsaka spremenljivka, definirana znotraj let*, ima dostop do prejšnjih definicij..po vrsti seta dol let vse naenkrat
             [v1 (fri (vars-e1 ex) environment)]
             [e2 (vars-e2 ex)]
             [novo;; kom zgori rabmo ker tuki mas s in v1
              ;; ideja od kolegov
              (cond
                [(and (list? s) (list? v1));morta bit isto dolga da lahko tlacmo skupi
                 (if (not (= (length s) (length v1)))
                     (triggered (exception "vars: mismatched lists"))
                     (let loop ([imena s] [seen '()])
                       (cond
                         [(null? imena) (map cons s v1)] ; cons poveze a in v1
                         [(member (car imena) seen) (triggered (exception "vars: duplicate identifier"))];nesme bit podvojenih
                         [else (loop (cdr imena) (cons (car imena) seen))])))]
                [(string? s) (list (cons s v1))];ko ena sama spr in ena vr
                [else (triggered (exception "vars: invalid arguments"))])])
        (cond
          [(triggered? v1) v1]  ; If `v1` is triggered, return it immediately
          [(triggered? novo) novo]  ; If `novo` is triggered, return it immediately
          [else (let ([extended-env (append novo environment)])  ; Extend the environment
             (fri e2 extended-env))]))];novo dodamo v env in evalviramo e2


     
     [(valof? ex)                     
      (let ([v (valof-s ex)])           
        (let loop ([env1 environment])              ; Definiramo rekurzivno funkcijo `loop` za iskanje v `env`
          (cond
            [(null? env1)                   
             (triggered (exception "valof: undefined variable"))]; ni not ni def
            [(equal? v (caar env1)) ;caar vzeme prvi par in vazame kljuc       
             (cdar env1)]                   ;iz prvega para vrne vrednost
            [else (loop (cdr env1))])))]   

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     [(list? ex)
      (map (lambda (x) (fri x environment)) ex)]

     

     
     [(fun? ex)
      (let* ([name (fun-name ex)]; ime funkcije
             [args1 (fun-args ex)];sez argumentov
             [body (fun-body ex)]);;telo funckije
        
        (if (list? args1)
           (if (equal? (length args1) (length (remove-duplicates args1)));razlicna imena
               (closure environment ex);;ex wtffffffff 
               (triggered (exception "fun: duplicate argument identifier")))
           (triggered (exception "fun: arguments must be a list"))))]
     
     [(proc? ex) ex]
     [(closure? ex) ex]
     
     ;;;pomagam ai 
     [(call? ex)
      (let* ([func (fri (call-e1 ex) environment)] ;funkcijo znotraj preoblikujemmo v ovojnico
             [arg-e2 (call-e2 ex)]
             [args (if (list? arg-e2)
                       (map (lambda (arg) (fri arg environment)) arg-e2);evelviramo vse arg
                       (list (fri arg-e2 environment)))]);ce samo en el
        (cond
          [(triggered? func) func]
          
          [(closure? func)
            (let* ([closure-env (closure-env func)] ;get the closure of the function
                   
                   [fun-def (closure-fun func)] ;get the fun definition from closure
                   [fun-args (fun-args fun-def)] ;get the fun arguments
                   
                   [num-args (length args)] ;the number of arguments passed
                   [num-fun-args (length fun-args)] ;the number of function parameters
                   
                   [arg-env (if (= num-args num-fun-args);ce dolzini enaki
                                (append (map cons fun-args args) (cons (cons (fun-name fun-def) func) closure-env));(cons 10 '(20 30))  ; => (10 20 30)
                                ;append ju zdruzi
                                (triggered (exception "call: arity mismatch")))])
                (fri (fun-body fun-def) arg-env))]
          
          [(proc? func)
           (fri (proc-body func) (cons (cons (proc-name func) func) environment))]
          
          [else (triggered (exception "call: wrong argument type"))]))]
     
    [else (error "Unknown expression" ex)]))


;; makri

(define (greater e1 e2)
  (?leq e2 e1))


(define (rev e)
  (vars (list "fun");definiramo fubkcijo
        (list (fun "fun" (list "zap" "acc"); zaporedje pa da se nabira fun sprejme te dva ragumenta
                   (if-then-else;body fun+
                    (?empty (valof "zap"))
                    (valof "acc")
                    (call (valof "fun");poklicemo funkcijo na argumentih
                          (list (tail (valof "zap")
                                      (.. (head (valof "zap"))(valof "acc"))))))))

        (call (valof "fun") (list e (empty)))))


;; probbly narobe
(define (binary e)
  (let ([v e])
    (cond
      [(and (int? v) (> (int-n v) 0))
       (let loop ([n (int-n v)] [result (empty)])
         (if (= n 0)
             result
             (loop (quotient n 2) (.. (int (remainder n 2)) result))))]
      [(triggered? v) v]
      [else (triggered (exception "binary: argument must be a positive integer"))])))

(define (mapping f seq)
  (let loop ([eval-f f]
             [eval-seq seq]
             [new-seq (empty)])
    (if (?seq eval-seq)
        (cond
          [(triggered? eval-seq) eval-seq]
          [(empty? eval-seq) (rev new-seq)]
          [(..? eval-seq)
           (let* ([head (..-e1 eval-seq)]
                  [tail (..-e2 eval-seq)]
                  [result (call eval-f (list head))])
             (if (triggered? result)
                 result
                 (loop eval-f tail (.. result new-seq))))])
        (triggered (exception "mapping: wrong argument type")))))

(define (filtering f seq)
  (let loop ([eval-f f]
             [eval-seq seq]
             [new-seq (empty)])
    (if (?seq eval-seq)
       (cond
          [(triggered? eval-seq) eval-seq]
          [(empty? eval-seq) (rev new-seq)]
          [(..? eval-seq)
           (let* ([head (..-e1 eval-seq)]
                  [tail (..-e2 eval-seq)]
                  [result (call eval-f (list head))])
             (cond
               [(triggered? result) result]
               [(true? result) (loop eval-f tail (.. head new-seq))]
               [else (loop eval-f tail new-seq)]))])
       (triggered (exception "filtering: wrong argument type")))))

(define (folding f init seq)
  (let loop ([eval-f f]
             [eval-init init]
             [eval-seq seq])
    (if (?seq eval-seq)
        (cond
          [(triggered? eval-init) eval-init]
          [(triggered? eval-seq) eval-seq]
          [(empty? eval-seq) eval-init]
          [(..? eval-seq)
           (let* ([head (..-e1 eval-seq)]
                  [tail (..-e2 eval-seq)]
                  [result (call eval-f (list head eval-init))])
             (if (triggered? result)
                 result
                 (loop eval-f result tail)))])
        (triggered (exception "folding: wrong argument type")))))



    






