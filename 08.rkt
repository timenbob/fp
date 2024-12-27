#lang racket

;podatkovi tipi

;(struct true () #:transparent);zmeri dodaj transperent

;(struct add (e1 e2) #:transparent)
;(match (add 1 2)[(add a b) (cons a b )]);treba je gnezdit deli brez pogojnih stavkov ce se le da
; mores gledat npr pru + da pregledas tipe
; zavedi se da negacija lahko dobi izraz in ne le vrendosti
; tuki ni izjem na sem je
; neki operatorji so lahko kratkosticni(za sem je nujno to gledat) za tuki ne ve
;

; preverjanje tipov
; rkt sam naredi funkcijo ?int to je struct in ne funkcija v rkt
; ne vsega v naprej racunat(da interpretor ne trpi)
; makroti niso od rkt ampak od nasga intepretorja

;(struct int(e)#:transparent)

; funkcije v recektu so makro za nas fri jezik

; makri ne klicejo fri interpretojrea
;;;;;;;;;;;;;;

(struct int (e) #:transparent )
(struct add (e1 e2) #:transparent )
(struct mul (e1 e2) #:transparent )

(struct true () #:transparent )
(struct false () #:transparent )
(struct if-then-else (condition e1 e2) #:transparent)
(struct ?leq (e1 e2) #:transparent)
(struct ~ (e1) #:transparent)
(struct ?int (e1) #:transparent)


(define (fri e)
    (match e
    [(or (true) (false) (int _)) e]
    [(add e1 e2)
        (match* ((fri e1) (fri e2))
        [((int i1) (int i2)) (int (+ i1 i2))]
        [((true) _) (true)]
        [((false) e) e])]
    [(mul e1 e2)
        (match* ((fri e1) (fri e2))
        [((int i1) (int i2)) (int (* i1 i2))]
        [((true) e) e]
        [((false) _) (false)])]
    [(?leq e1 e2)
        (match* ((fri e1) (fri e2))
        [((int i1) (int i2)) #:when (<= i1 i2) (true)]
        [(_ _) (false)])]
    [(if-then-else condition e1 e2)
        (if (true? (fri condition)) (fri e1) (fri e2))]
    [(~ e1)
     (match (fri e1)
       [(int i) (int (- i))]
       [(true) (false)]
       [(false) (true)])]
     [(?int e)
      (match (fri e)
        [(int _) (true)]
        [_ (false)])]


        
   
))

(define (conditional . clauses) ;The . before clauses means this function accepts a variable number of arguments.
  (cond
    [(null? clauses) (error "No clauses provided")]
    [(= (length clauses) 1) (error "Incomplete clause pair")]
    [else (foldr (lambda (clause rest)
                   (if (pair? rest)
                       `(if-then-else ,(car clause) ,(cadr clause) ,rest)
                       clause))
                 (last clauses)
                 (take clauses (- (length clauses) 2)))]))


(define (?geq e1 e2) (?leq e2 e1))

; klic v terminalu  (fri (add(add(int 1)(int -1)) (mul(int 10) (int 20))))

;(fri (add(add(int 1)(int -1)) (mul(int 10) (int 20))))
;(fri (true))

; uporabli trace v navodilih za seminarsko kaze kaj dela ce gre v lup


;int-e ne uporabi ker ne ves ali je res integer


;POGLEJ V DISCORD

; SEMINARSKA


; treba si je pogledat razlicne matche

; izjeme! isto kot ostali prog jeziki ne ustvarji novo

; ?.. preveri ce je .. na zacetku
; ? seq preveri zeres

; ujemanje  izjeme so sranje    ostalo ce je res enako in lahko uporabis enakost iz rkt(poglej kateri je pravi ker je velik razlicnih tisti ki rekurzivno preverja ))

; ekstrakcij

; nasprotna vrednost izjeme spet pazi

; all naki alal konjunkcija
; any 
; ne gres preventivno se rcunat ustavi sproti(ce pride izjema)

;SPREMENJIVKE

; sencenje(zaradi dodajanja...dodaji na cazetek seznama in je trivialno)
; lokalno okolje deluje
; vars s je lahko seznam ce je sezna je tudi e1 seznam
;        ce ni seznam .... se racuna e2 RAZSIRIMO OKOOLJE.... tuki mas sencenje NESME MET DUPLIKTOV(vrni napako)



;valof(ce pravilno dodajas v seznam + pravilni red bi moglo ok delat


;FUCKIJE

;funckije leksikografski doseg

; procedura ne shrani okolja in samo jolo dinamicni doseg

; funcija se poracuna v funckijsko ovojnico  procedure tega nimajo...v ta namen imas zato closure
; na kocu optimiziraj ce sploh

; funcije nimao duplicirnaih ele vrni error

; klicaje funkcij... dost argumentov
; okolje bo treba razsirit z imeni vseh vrednsotimi argumentov
; treba bo dodat ime funkcije in OVOJNICO(da mamo lahko rekurzijo)
; ovojnica je tipo ala spremenjivka

; najprej dodoaj okolju vezave ookoloje na koncu pa spremenjivke ..
; spremenlijvke dodajanje vrsti red ni pomemeben ker ni dupliciranih
; pejdi od prve go zadnjega



; procedura nima argumentov
; imajk ime na voljo
; od na sodvisno  a bo rekurzivno ja al ne(on nima zares testa za to
; a je procedura rekurzivna ja al ne samo neki narediiii in vedi kaj dela a je rekurzija al ni


; NAPOTKI
; undefined variable bo delal le ko naredis drugi del se


; MAKRI
; so funcije v reketu
; samo manipulirajo kodo
; cilj je da pokazemo da znamo programirat v jeziku fr
; argumenti le 1x izracunani!
; testiran na njegovi kodi
; ne klict fri interprorja  minus tocke
; tudi rkt makroti ne pridjo v postev

; dvojiski zapis je baje hard(ker ni deljenja)






































