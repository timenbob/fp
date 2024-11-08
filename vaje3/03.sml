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
        fun pomozna([], (accA, accB)) = (rev accA, rev accB) (*rev obrne*)
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

fun rotate2(tree: 'a bstree, dir: direction): 'a bstree =
    case (tree, dir) of
        (lf, _) => lf 
      | (br(left, root, right), L) =>
            (case right of
                lf => br(left, root, lf)
              | br(rLeft, rRoot, rRight) => br(br(left, root, rLeft), rRoot, rRight)
            )
      | (br(left, root, right), R) =>
            (case left of
                lf => br(lf, root, right)
              | br(lLeft, lRoot, lRight) => br(lLeft, lRoot, br(lRight, root, right))
            );

fun rotate (br (l, x, br (rl, rx, rr)), L) = br (br (l, x, rl), rx, rr)
  | rotate (br (br (ll, lx, lr), x, r), R) = br (ll, lx, br (lr, x, r))
  | rotate (t, _) = t;  

fun height lf = 0
  | height (br (l, _, r)) = 1 + Int.max (height l, height r);

fun rebalance (br (l, x, r)) =
  let
    val lh = height l
    val rh = height r
  in
    if lh > rh + 1 then
      case l of
          br (ll, lx, lr) =>
            if height ll >= height lr then rotate (br (l, x, r), R)
            else rotate (br (rotate (l, L), x, r), R)
        | lf => br (l, x, r)  
        else if rh > lh + 1 then
      case r of
          br (rl, rx, rr) =>
            if height rr >= height rl then rotate (br (l, x, r), L)
            else rotate (br (l, x, rotate (r, R)), L)
        | lf => br (l, x, r)  
        else br (l, x, r)  
  end;

datatype order = LESS | EQUAL | GREATER;

fun c(a, b) =
  if a < b then LESS
  else if a > b then GREATER
  else EQUAL;


fun avl (c, lf, e) = 
    br (lf, e, lf)
  | avl (c, br (l, x, r), e) =
      case c (e, x) of
          LESS => rebalance (br (avl (c, l, e), x, r))
        | GREATER => rebalance (br (l, x, avl (c, r, e)))
        | EQUAL => br (l, x, r); 

