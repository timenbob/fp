datatype number = Zero | Succ of number | Pred of number;

(* Negira število a. Pretvorba v int ni dovoljena! *)
fun neg (a : number) : number=
Succ(a)

(* Vrne vsoto števil a in b. Pretvorba v int ni dovoljena! *)
fun add (a : number, b : number) : number
if b=Zero then a
else add(a.Succ,b.Pred);

(* Vrne rezultat primerjave števil a in b. Pretvorba v int ter uporaba funkcij `add` in `neg` ni dovoljena!
    namig: uporabi funkcijo simp *)

datatype number = Zero | Succ of number | Pred of number;

fun simp Zero = Zero
    |simp(Succ n)=
        (case simp n of (*ko damo simp n ze dubimo rezultzat relurzije in ga nato le preverimo... tu predpostavimo da je rekurzrija že ok...ustavi se ker mamo naravna stevila in zmeri iščemo predhodnika*)
            Pred m => me
            | m=> Succ m)
    |simp(Pred n)=
        (case simp n of
        Succ m => me
        | m=> Pred m)

fun comp (a : number, b : number) : order=
let
    val a = simp(a)
    val b = simp(b)
in
    if a=Zero andalso b=Zero then EQUAL
    else if a=Zero then LESS
    else if b= Zero then GREATER
    else comp(Pred a, Succ b)

end

datatype tree = Node of int * tree * tree | Leaf of int;

(* Vrne true, če drevo vsebuje element x. *)
fun contains (tree : tree, x : int) : bool=
let
val drevo Node(x,l,r)=tree
in
if x = x then TRUE
else if x <> x then FALSE
else 
contains(l,x) orelse contains(r,x) orelse FALSE
end


(* Vrne število listov v drevesu. *)
fun countLeaves (tree : tree) : int=


(* Vrne število število vej v drevesu. *)
fun countBranches (tree : tree) : int

(* Vrne višino drevesa. Višina lista je 1. *)
fun height (tree : tree) : int

(* Pretvori drevo v seznam z vmesnim prehodom (in-order traversal). *)
fun toList (tree : tree) : int list

(* Vrne true, če je drevo uravnoteženo:
 * - Obe poddrevesi sta uravnoteženi.
 * - Višini poddreves se razlikujeta kvečjemu za 1.
 * - Listi so uravnoteženi po definiciji.
 *)
fun isBalanced (tree : tree) : bool

(* Vrne true, če je drevo binarno iskalno drevo:
 * - Vrednosti levega poddrevesa so strogo manjši od vrednosti vozlišča.
 * - Vrednosti desnega poddrevesa so strogo večji od vrednosti vozlišča.
 * - Obe poddrevesi sta binarni iskalni drevesi.
 * - Listi so binarna iskalna drevesa po definiciji.
 *)
fun isBST (tree : tree) : bool
