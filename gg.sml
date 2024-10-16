(*  Vrne fakulteto števila n, n >= 0. *)
fun factorial (n : int) =
if n==1
then 1
else n*factorial(n-1);

(*  Vrne n-to potenco števila x, n >= 0. *)
fun power (x : int, n : int)=
if n==1
then x
else x*power(n-1);

(*  Vrne največjega skupnega delitelja pozitivnih števil a in b, a >= b. *)
fun gcd (a : int, b : int)=
if b==0
then a
else gcd(b,a mod b);

(*  Vrne dolžino seznama. *)
fun len (xs : int list)=
if null xs
then 0
else 1+len(tl xs);

(*  Vrne SOME zadnji element seznama. Če je seznam prazen vrne NONE. *)
fun last (xs : int list)=
if null xs
then NONE
let
val a = hd xs
val b = tl xs 
in
if null b
then a
else last(b)
end

(*  Vrne SOME n-ti element seznama. Prvi element ima indeks 0. Če indeks ni veljaven, vrne NONE. *)
fun nth (xs : int list, n : int) : int option

(*  Vrne nov seznam, ki je tak kot vhodni, le da je na n-to mesto vrinjen element x. Prvo mesto v seznamu ima indeks 0. Indeks n je veljaven (0 <= n <= length xs). *)
fun insert (xs : int list, n : int, x : int) : int list

(*  Vrne nov seznam, ki je tak kot vhodni, le da so vse pojavitve elementa x odstranjene. *)
fun delete (xs : int list, x : int) : int list

(*  Vrne obrnjen seznam. V pomoč si lahko spišete še funkcijo append, ki doda na konec seznama. *)
fun reverse (xs : int list) : int list

(*  Vrne true, če je podani seznam palindrom. Tudi prazen seznam je palindrom. *)
fun palindrome (xs : int list) : bool
