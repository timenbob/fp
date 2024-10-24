(* Vrne naslednika števila `n`. *)
fun next (n : int) = n+1;

(* Vrne vsoto števil `a` in `b`. *)
fun add (a : int, b : int) = a+b;

(* Vrne true, če sta vsaj dva argumenta true, drugače vrne false *)
fun majority (a : bool, b : bool, c : bool) =  
 (a andalso b) orelse (a andalso c) orelse (b andalso c);


(* Vrne mediano argumentov - števila tipa real brez (inf : real), (~inf : real), (nan : real) in (~0.0 : real)
   namig: uporabi Real.max in Real.min *)
fun median (a : real, b : real, c : real) = 
(*a+b+c-Real.max(Real.max(a,b),c)-Real.min(Real.min(a,b),c);*)
Real.max(Real.max(Real.min(a,b),Real.min(a,c)),Real.min(b,c));


(* Preveri ali so argumenti veljavne dolžine stranic nekega trikotnika - trikotnik ni izrojen *)
fun triangle (a : int, b : int, c : int) =
(a+b)>c andalso (a+c)>b andalso (c+b)>a;

val test_majority = majority(true, true, false);
