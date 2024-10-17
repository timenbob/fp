fun implies' (a,b)=not a orelse b;
fun implies (z:bool*bool):bool=
    not(#1 z)orelse(#2 z);

fun partition (x, xs) =
let
    fun partition (xs, l, r) =
        if null xs then (rev l, rev r)
        else if hd xs < x
        then partition(tl xs,hd xs :: l,r)
        else partition(tl xs,l,hd xs::r)
in
    partition (xs, [], [])
end;

(*
fun quickSelect (k : int, xs : int list) : int option=
(* ali pa fun quickSelect (k : int, xs : int list) : int option *)
if null xs then NONE else (*ta else vrne to ka je dol*)
let
    val a = hd xs
    val p = partition(a,tl xs)
    val l = #1 p
    val r = #2 p
    fun len(c,s)= 
        if nal s then c 
        else len(c+1,tl s)
    val len_l = len(0,l)
in
if len_l=k 
        then SOME a (* tuki damo some in ne gor pred else ker drugace spodnnji dve data some some int*)
        else
            if len_l>k
            then quickSelect(k,l)
            else quickSelect(k-len_l-1,r)
end;

*)

(*  Vrne fakulteto števila n, n >= 0. *)
fun factorial (n : int):int =
if n=1
then 1
else n*factorial(n-1);

(*  Vrne n-to potenco števila x, n >= 0. *)
fun power (x : int, n : int)=
if n=1
then x
else x*power(x,n-1);

(*  Vrne največjega skupnega delitelja pozitivnih števil a in b, a >= b. *)
fun gcd (a : int, b : int)=
if b=0
then a
else gcd(b,a mod b);

(*  Vrne dolžino seznama. *)
fun len (xs : int list)=
if null xs
then 0
else 1+len(tl xs);

(*  Vrne SOME zadnji element seznama. Če je seznam prazen vrne NONE. *)
fun last (xs : int list):int option=
if null xs then NONE 
else
    let
    val a = hd xs
    val b = tl xs 
    in
    if null b
    then SOME a
    else last(b)
    end;

(*  Vrne SOME n-ti element seznama. Prvi element ima indeks 0. Če indeks ni veljaven, vrne NONE. *)
fun nth (xs : int list, n : int) : int option=
if null xs then NONE 
else 
    if null (tl xs) 
    then SOME (hd xs)
    else nth(tl xs,n-1); 

(*  Vrne nov seznam, ki je tak kot vhodni, le da je na n-to mesto vrinjen element x. Prvo mesto v seznamu ima indeks 0. Indeks n je veljaven (0 <= n <= length xs). *)
fun insert (xs : int list, n : int, x : int) : int list=
if (n=0)
then x::insert(tl xs,~1,x)
else (hd xs)::insert(tl xs, n-1,x);


(*  Vrne nov seznam, ki je tak kot vhodni, le da so vse pojavitve elementa x odstranjene. *)
fun delete (xs : int list, x : int) : int list=
if (hd xs=x) 
then delete(tl xs,x)
else (hd xs)::delete(tl xs,x);

(*  Vrne obrnjen seznam. V pomoč si lahko spišete še funkcijo append, ki doda na konec seznama. *)
fun reverse (xs : int list) : int list=


(*  Vrne true, če je podani seznam palindrom. Tudi prazen seznam je palindrom. *)
fun palindrome (xs : int list) : bool

