signature RATIONAL =
sig
    (* Definirajte podatkovni tip rational, ki podpira preverjanje enakosti. *)
    eqtype rational
    (* Definirajte izjemo, ki se kliče pri delu z neveljavnimi ulomki - deljenju z nič. *)
    exception BadRational
    (* Vrne racionalno število, ki je rezultat deljenja dveh podanih celih števil. *)
    val makeRational: int * int -> rational
    (* Vrne nasprotno vrednost podanega števila. *)
    val neg: rational -> rational
    (* Vrne obratno vrednost podanega števila. *)
    val inv: rational -> rational
    (* Funkcije za seštevanje in množenje. Rezultat vsake operacije naj sledi postavljenim pravilom. *)
    val add: rational * rational -> rational
    val mul: rational * rational -> rational
    (* Vrne niz, ki ustreza podanemu številu.
        Če je število celo, naj vrne niz oblike "x" oz. "~x".
        Če je število ulomek, naj vrne niz oblike "x/y" oz. "~x/y". *)
    val toString: rational -> string
end

structure Rational : RATIONAL=
struct
    (* Podatkovni tip rational lahko predstavlja bodisi celo število bodisi ulomek. *)
    datatype rational = 
         Int of int  (* celo število *)
        | Ulomek of int * int  (* ulomek *)

    (* Funkcija za izračun največjega skupnega delitelja (GCD). *)
    fun gcd(a, b) = 
        if b = 0 then a
        else gcd(b, a mod b)

    (* Funkcija za okrajšanje ulomka. *)
    fun okrajsaj(x, y) = 
    let 
        val g = gcd(x, y)
    in
        (x div g, y div g)
    end


    (* Preverimo, če je ulomek neveljaven (imenovalec mora biti večji od 1). *)
    exception BadRational

    (* Funkcija za ustvarjanje racionalnega števila. *)
    fun makeRational(x, y) = 
        if y = 0 then raise BadRational
        else if y = 1 then Int(x)
        else
            let
                val (st, imen) = okrajsaj(x, y)
            in
                Ulomek(st, imen)
            end;

    (* Funkcija za nasprotno vrednost racionalnega števila. *)
    fun neg (Int(x)) = Int(~x)
      | neg (Ulomek(x, y)) = makeRational(~x, y)

    (* Funkcija za obratno vrednost racionalnega števila. *)
    fun inv (Int(x)) = if x = 0 then raise BadRational else Ulomek(1, x)
      | inv (Ulomek(x, y)) = 
            if x = 0 then raise BadRational 
            else Ulomek(y, x)

    (* Funkcija za seštevanje racionalnih števil. *)
    fun add (Int(x), Int(y)) = Int(x + y)
      | add (Int(x), Ulomek(y, z)) = 
            Ulomek(x * z + y, z)
      | add (Ulomek(x, y), Int(z)) = 
            Ulomek(x + z * y, y)
      | add (Ulomek(x, y), Ulomek(z, w)) = 
            Ulomek(x * w + z * y, y * w)

    (* Funkcija za množenje racionalnih števil. *)
    fun mul (Int(x), Int(y)) = Int(x * y)
      | mul (Int(x), Ulomek(y, z)) = 
            Ulomek(x * y, z)
      | mul (Ulomek(x, y), Int(z)) = 
            Ulomek(x * z, y)
      | mul (Ulomek(x, y), Ulomek(z, w)) = 
            Ulomek(x * z, y * w)

    (* Funkcija za pretvorbo racionalnega števila v niz. *)
    fun toString (Int(x)) = Int.toString(x)
      | toString (Ulomek(x, y)) = 
            Int.toString(x) ^ "/" ^ Int.toString(y)
end


(*======================================*)

signature EQ =
sig
    type t
    val eq : t -> t -> bool
end

signature SET =
sig
    (* podatkovni tip za elemente množice *)
    type item
    (* podatkovni tip množico *)
    type set
    (* prazna množica *)
    val empty : set
    (* vrne množico s samo podanim elementom *)
    val singleton : item -> set
    (* unija množic *)
    val union : set -> set -> set
    (* razlika množic (prva - druga) *)
    val difference : set -> set -> set
    (* a je prva množica podmnožica druge *)
    val subset : set -> set -> bool
end

funsig SETFN (Eq : EQ) = SET

functor SetFn (Eq : EQ) : SET =
struct
    type item = Eq.t  
    
    type set = item list 

    val empty : set = []

    fun singleton (x:item):set = [x];

    fun add x [] = [x]
      | add x (y::ys) =
            if Eq.eq x y then y::ys
            else y :: add x ys

    fun union xs ys = foldl (fn (x, acc) => add x acc) ys xs

    fun difference [] _ = []
      | difference (x::xs) ys =
            if List.exists (fn y => Eq.eq x y) ys then difference xs ys
            else x :: difference xs ys

    fun subset [] _ = true
      | subset (x::xs) ys =
            List.exists (fn y => Eq.eq x y) ys andalso subset xs ys
end




