fun next (n:int)=n+1

fun add(a,b)=a+b

val vr=add(2,3)

fun factorial(n)=
    let
        fun pomoz(n,acc)=
            if n=0 then acc
            else pomoz(n-1,acc*n)
    in
    pomoz(n,1)
    end

val vr1=factorial(5)

fun len (xs : int list) : int=
    let
        fun pomoz([],acc)=acc
        |pomoz(x::xs,acc)=pomoz(xs,acc+1)
            
    in
    pomoz(xs,0)
    end

val vr2=len([1,2,3,4,5])

fun last (xs : int list) : int option=
    let
        fun pomoz([])=NONE
        |   pomoz(x::[])=SOME x
        |   pomoz(x::xs)=pomoz(xs)

    in
        pomoz(xs)
    end

val vr3=last([1,2,3,4,5])
val vr4=last([])
    


datatype tree= Node of int*tree*tree
                | Leaf of int

fun contains (tree, x)=
let
    fun poz(Leaf V,x)= V=x
    |poz(Node(k,l,d),x)=(k=x)orelse(poz(l,x))orelse(poz(d,x)) 
in
    poz(tree,x)
end


fun map(f,sez)=
    foldr (fn (x,acc) => f x :: acc) [] sez

fun f x=x+1

val gg=map(f,[1,2,3])

fun zip(sez1, sez2)=
    let
    fun pomoc([],y,acc)=acc
    |pomoc(x,[],acc)=acc
    |pomoc(x::xs,y::xy,acc)=pomoc(xs,xy,(x,y)::acc)
    in
    pomoc(sez1,sez2,[])
    end    



(*currying*)
val onlyEven  = List.filter(fn x=>x mod 2=0) 

val h=onlyEven([1,2,3,4,5,6,7])


fun bestString f [] =""
    | bestString f [x]=x
    | bestString f (h1::h2::tl) = 
        if f(h1,h2) then bestString f (h1::tl)
        else bestString f (h2::tl)


datatype 'a set = elem of {a:'a, b:'a set} | empty
fun fun1 x y =
case y of
SOME(z) => elem {a=z, b=empty}
| NONE => empty

datatype ('a, 'b) set = elem of 'a list * ('a, 'b) set | empty of 'b
fun fun1 (x, y) [z] =
case y of
elem ([nil], empty z) => x
| _ => SOME z

val r1 = {artikel="kruh", cena_enote=2.5, enot=4}
val r2 = {artikel="mleko", cena_enote=1.2, enot=3}
val r3 = {artikel="sir", cena_enote=3.4, enot=5}
val nakup = [("branko", [r1,r2]), ("natasa", [r2,r3]), ("branko", [r3])]



fun a b c =
case b of
{d=u, e=v} => if (u=1) andalso (v=false)
then 3.14
else c b

fun f x y z = List.foldl (fn (g1::g2::r, y) => SOME(g2+(valOf y)))

fun f1(SOME a) = f2(valOf a)
and f2(b)
= 2*b;


fun x {a=b, c=d} h =
case (b,d) of
(SOME e, f::g) => e andalso f andalso (x {a=b, c=g} h )
| (NONE, f::g) => f andalso (x {a=b, c=g} h)
| _ => h


(*  ZAKAZNITEV SPROZITEV *)
(*(define (ifvsotafun2 e1 e2 e3 e4)
  (let* ((dvojni-e1 (+ e1 e1))
         (dvojni-e2 (+ e2 e2))
         (rezultat-e3 (delay (e3)))  ; zakasnitev klica e3
         (rezultat-e4 (delay (e4)))) ; zakasnitev klica e4
    (if (> dvojni-e1 30)
        (+ dvojni-e2 (force rezultat-e3))   ; e3 se izračuna enkrat
        (+ (force rezultat-e3) (force rezultat-e3) (force rezultat-e4) (force rezultat-e4) (force rezultat-e4))))) ; e3 in e4 se evalvirata večkrat po potrebi
*)
