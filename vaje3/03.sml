datatype natural = Succ of natural | One;
exception NotNaturalNumber of natural;

datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

(*---------------------------------------------------*)

fun zip (x: 'a list, y: 'b list): ('a * 'b) list =
    let 
        (* Helper function that takes two lists and an accumulator *)
        fun pomozna([], _, acc) = acc
          | pomozna(_, [], acc) = acc
          | pomozna(x::xs, y::ys, acc) = pomozna(xs, ys, acc @ [(x, y)])
    in
        pomozna(x, y, [])
    end;

fun unzip (ab : ('a * 'b) list) : 'a list * 'b list =
    let
        fun pomozna([], (accA, accB)) = (rev accA, rev accB) (*rev reverses order*)
          | pomozna((a, b)::rep, (accA, accB)) = pomozna(rep, (a::accA, b::accB))
    in
        pomozna(ab, ([], []))
    end

fun subtract (One : natural, _ : natural) = raise NotNaturalNumber One
  | subtract (Succ a, One) = a
  | subtract (Succ a, Succ b) = subtract(a, b);

fun any (_ : ('a -> bool) , []: 'a list)=false
    |any(f,a)= if f(hd a) then true else any(f,tl a);

fun map(_ : ('a -> 'b), []:'a list) = []
    | map(f,a)=f(hd a)::map(f,tl a);

fun filter(_ : ('a -> bool), []:'a list)=[]
    |filter(f,s)= if f(hd s) then hd s::filter(f,tl s) else filter(f,tl s);

fun fold(f, z, s)=
    case s of 
        []=>z 
    |glava::rep=>fold(f,f(z,glava),rep);

datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

fun rotate(tree: 'a bstree, dir: direction): 'a bstree =
    case (tree, dir) of
        (lf, _) => lf  (* If the tree is empty, return empty *)
      | (br(left, root, right), L) =>
            (* Perform a right rotation *)
            (case right of
                lf => br(left, root, lf)  (* Right is empty, can't rotate *)
              | br(rLeft, rRoot, rRight) => br(br(left, root, rLeft), rRoot, rRight)
            )
      | (br(left, root, right), R) =>
            (* Perform a left rotation *)
            (case left of
                lf => br(lf, root, right)  (* Left is empty, can't rotate *)
              | br(lLeft, lRoot, lRight) => br(lLeft, lRoot, br(lRight, root, right))
            );


