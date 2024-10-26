datatype number = Zero | Succ of number | Pred of number;

(* Negira število a. Pretvorba v int ni dovoljena! *)
fun neg (a : number) : number=
case a of
    Zero => Zero
    |Pred n =>Succ(neg(n))
    |Succ n=>Pred(neg(n))

(* Negate the number *)
fun neg2 (Zero) = Zero
  | neg2 (Succ n) = Pred (neg n)
  | neg2 (Pred n) = Succ (neg n);

(* Vrne vsoto števil a in b. Pretvorba v int ni dovoljena! 
if b=Zero then a
else add(Succ a,Pred b);*)
fun add (a : number, b : number) : number =
    case b of
        Zero => a
      | Succ b => add (Succ a, b)
      | Pred b => add (Pred a, b);

fun add2 (a : number, Zero) = a
  | add2 (a : number, Succ b) = add2 (Succ a, b)
  | add2 (a : number, Pred b) = add2 (Pred a, b);


(* Vrne rezultat primerjave števil a in b. Pretvorba v int ter uporaba funkcij `add` in `neg` ni dovoljena!
    namig: uporabi funkcijo simp *)


fun simp Zero = Zero
  | simp (Succ n) =
      (case simp n of
          Zero => Succ Zero
        | Pred m => m  (* Simplifies a successor followed by a predecessor *)
        | m => Succ m)
  | simp (Pred n) =
      (case simp n of
          Zero => Pred Zero
        | Succ m => m  (* Simplifies a predecessor followed by a successor *)
        | m => Pred m);

fun comp (a : number, b : number) : order =
    let
        val sa = simp a
        val sb = simp b
    in
        if sa = Zero andalso sb = Zero then EQUAL
        else if sa = Zero then LESS
        else if sb = Zero then GREATER
        else comp (Pred sa, Succ sb)

        case sa,sb of 
             sa = Zero andalso sb = Zero => EQUAL
            |sa = Zero andalso 
    end;


datatype tree = Node of int * tree * tree | Leaf of int;

(* Vrne true, če drevo vsebuje element x. *)
fun contains (Leaf v, x) = (v = x)
  | contains (Node (v, l, r), x) =
      if v = x then true
      else contains (l, x) orelse contains (r, x);


(* Vrne število listov v drevesu. *)
fun countLeaves (Leaf v) = 1
  | countLeaves (Node (v, l, r)) = countLeaves(l) + countLeaves(r);



(* Vrne število število vej v drevesu. *)
fun countBranches (Leaf v) = 0
  | countBranches (Node (v, l, r)) = 2+ countBranches (l) + countBranches (r);



(* Vrne višino drevesa. Višina lista je 1. *)
fun height (Leaf _) = 1
    |height(Node (v, l, r)) = 1+ Int.max(height (l) , height (r));



(* Pretvori drevo v seznam z vmesnim prehodom (in-order traversal). *)
fun toList (Leaf v) = [v]
  | toList (Node (v, l, r)) = toList(l) @ [v] @ toList(r);


(* Vrne true, če je drevo uravnoteženo:
 * - Obe poddrevesi sta uravnoteženi.
 * - Višini poddreves se razlikujeta kvečjemu za 1.
 * - Listi so uravnoteženi po definiciji.
 *)
fun isBalanced (Leaf v) = true
    |isBalanced  (Node (v, l, r)) = abs(height(l) - height(r))<= 1andalso isBalanced l andalso isBalanced r;



(* Vrne true, če je drevo binarno iskalno drevo:
 * - Vrednosti levega poddrevesa so strogo manjši od vrednosti vozlišča.
 * - Vrednosti desnega poddrevesa so strogo večji od vrednosti vozlišča.
 * - Obe poddrevesi sta binarni iskalni drevesi.
 * - Listi so binarna iskalna drevesa po definiciji.
 *)
fun isBST (Leaf v) = true
    |isBST (Node (v, l, r)) =
    let

          fun max1(Leaf x) = x
            | max1 (Node (x, _, r)) = max1(r)

          (* Check that all values in the right subtree are greater than v *)
          fun min1 (Leaf x) = x
            | min1 (Node (x, l, _)) = min1(l)
      in
          max1(l)<v andalso min1(r)>v
            (*se levo in desno*)
          andalso 
          isBST l 
          andalso 
          isBST r
      end;


