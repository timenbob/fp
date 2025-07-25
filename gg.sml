fun x {a=b, c=d} h =
    case (b,d) of
    (SOME e, f::g) => e andalso f andalso (x {a=b, c=g} h )
    | (NONE, f::g) => f andalso (x {a=b, c=g} h)
    | _ => h;

fun f (a,b) c =
let val (b1,b2) = b
in if a > 3 andalso b2
then c b1
else c b2
end

val x = 3
val w = fn _ => x
fun q x =
let val x = 4
in w
end
val rez1 = q 30 15
fun w x = x + 1
val rez2 = q 31 14

fun f1 x y z = (x andalso y, z)
fun f2 x y z = (x andalso z, y)
fun f3 x y z = (y andalso z, x)
val x = f1 true;
val x = f1 true true; (*ta ni ok*)
val x = f1 true true true;
val x = f2 true; (*ta ni ok*)
val x = f2 true true;
val x = f2 true true true;
val x = f3 true;
val x = f3 true true;
val x = f3 true true true;

(*
fun sestaj a b=
let 
    fun vsota []=0
    | vsoat hd::tl=hd+vosta(tl)

in
if a>0 then NONE else SOME vsota(b+a)

end

val sestej3 = sestej 0 [3, 3, 3];*)

datatype 'a Tip1 = A of 'a
                | B of int


fun poljubno(a,A b)=a(*A b pomeni, da bomo ujemali vrednost, ki je znotraj konstruktorja A, in ta vrednost bo shranjena v spremenljivki b.*)














fun sz (stevila, n)=
    let
    fun reku([], n, acc)=acc
        |reku(st, 0, acc)=acc
        |reku(hd::tl, n, acc)= if hd>0 then reku(tl, n-1, acc+hd) else reku(tl, n, acc)

    in
    reku(stevila n 0)
    end


fun tip a (b, c) =
case (b, a) of
(_, (SOME c)::x) => x
| _ => tl a


(*python zakasnjenno
def exponential_approximation(x):
    # Začnemo z začetnim členom (n=0), ki je 1
    n = 0
    current_sum = 1  # Začetni člen: 1
    while True:
        # Dodajamo naslednji člen zaporedja
        current_term = (x ** n) / math.factorial(n)
        current_sum += current_term
        yield current_sum  # Vrnemo trenutni približek
        n += 1
*)

fun deli(p, []) = ([], [])
|
deli(p, x :: xs) =
let
val (b, a) = deli(p, xs)
in
if x < p
then (x :: b, a)
else (b, x :: a)
end;

fun fold (f, acc, sez) =
    case sez of
        [] => acc
      | glava::rep => fold(f, f(glava, acc), rep)

fun s2 sez =
    let 
        val n = fold (fn (_, acc) => acc + 1, 0, sez) (* Count elements in the list *)
        val sum_x = fold (fn (x, acc) => x + acc, 0, sez) (* Compute sum of elements *)
        val sum_x2 = fold (fn (x, acc) => x*x + acc, 0, sez) (* Compute sum of squares *)
    in
        if n <= 1 then 0.0 (* Avoid division by zero *)
        else (real sum_x2 - (real sum_x * real sum_x) / real n) / real (n - 1)
    end



(*(define (dodaj tree n)
  (cond
    [(null? tree) (bstdrevo null n null)] ; Če je drevo prazno, ustvarimo novo vozlišče
    [(< n (bstdrevo-value tree)) 
     (bstdrevo (dodaj (bstdrevo-left tree) n) (bstdrevo-value tree) (bstdrevo-right tree))] ; Vstavimo v levo poddrevo
    [(> n (bstdrevo-value tree)) 
     (bstdrevo (bstdrevo-left tree) (bstdrevo-value tree) (dodaj (bstdrevo-right tree) n))] ; Vstavimo v desno poddrevo
    [else tree] ; Če je `n` že v drevesu, ga ne dodajamo
  ))
*)

datatype 'a Tip = A of (int * 'a)
| B of ('a -> int)
fun mojamoja a b =
case a of
B x => B x
| _ => A(3, 3)

fun mf x y =
case x y of
NONE => (fn y => [])

fun mf x y =
case (x,y) of
(a, _) => a [3]


(*pytho tok

z fuknicjo
def postevanka(n, seznam):
    for faktor in seznam:
        for i in range(1, n+1):
            yield (faktor, i, faktor * i)

# Testiranje generatorja
for result in postevanka(10, [2, 3]):
    print(result)

# Generator z uporabo izraza
postevanka = ((faktor, i, faktor * i) for faktor in [2, 3] for i in range(1, 11))

# Testiranje generatorja
for result in postevanka:
    print(result)

*)

datatype ('a, 'b) set = elem of 'a list * ('a, 'b) set | empty of 'b
fun fun1 (x, y) [z] =
case y of
elem ([nil], empty z) => x
| _ => SOME z

