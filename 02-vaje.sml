(*kvatropivsak
datatype barva = Kriz | Pik | Srce | Karo
datatype stopnja = As | Kralj | Kraljica | Fant | Stevilka of int

type karta = stopnja * barva

fun barvaKarte ((s, b) : karta) = b;

fun veljavnaKarta ((Stevilka i, b) : karta) = 
    i==> if 1<i andslso i<10
    | (As|Kralj|kraljica|Fant)=>true;

fun veljavnaKarta2 ((k,_) : karta) = 
case k of
    Stevilka i => if 1<i andslso i<10
    | ( _ )=>true;

fun vrednostKarte ((k , _) : karta) = case k of 
    Stevilka i => i 
    | As => 20
    | _ => 10;

fun vsotaKart ([] : karta list) = 0 | vsotaKart (k :: karte) = vrednostKarte k + vsotaKart karte;

fun isteBarve([] | [_]) =true
    | isteBarve ( ((_,b1)::(ks as ((_,b2)::_ ))): karta list) = 
    b1 = b2 andalso isteBarve ks;
*)

(*induktivni podatkovni tipi*)

datatype number = Zero | Succ of number | Pred of number

fun simp Zero = Zero
    |simp(Succ n)=
        (case simp n of (*ko damo simp n ze dubimo rezultzat relurzije in ga nato le preverimo... tu predpostavimo da je rekurzrija že ok...ustavi se ker mamo naravna stevila in zmeri iščemo predhodnika*)
            Pred m => me
            | m=> Succ m)
    |simp(Pred n)=
        (case simp n of
        Succ m => me
        | m=> Pred m)

datatype tree = Node of int * tree * tree | Leaf of int

fun min (Leaf x)=x
    |min(Node(x,l,r))=Int.min(x,Int.x(min l,min r))
min(Node (4, Leaf 5, Node(14,Leaf ~1, Leaf 5)))