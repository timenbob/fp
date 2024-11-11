datatype natural = Succ of natural | One;
exception NotNaturalNumber of natural;

datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

(*datatype order = LESS | EQUAL | GREATER;*)

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
    end;

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

fun rotate3 (br (l, x, br (rl, rx, rr)), L) = br (br (l, x, rl), rx, rr)
  | rotate3 (br (br (ll, lx, lr), x, r), R) = br (ll, lx, br (lr, x, r))
  | rotate3 (t, _) = t;  

fun rotate(drevo: 'a bstree, smer: direction) : 'a bstree =
    case (drevo, smer) of 
        (lf, _) => lf 
      | (br(left, value, right), L) => 
            (case right of 
                lf => drevo  
              | br(rL, rValue, rR) => 
                    br(br(left, value, rL), rValue, rR)) 
      | (br(left, value, right), R) =>
            (case left of 
                lf => drevo 
              | br(lL, lValue, lR) => 
                    br(lL, lValue, br(lR, value, right))); 


fun height (tree ) = 
    case tree of
        lf => 1
        | br (l,_,r) => 
            let 
                val left = height(l)
                val right = height(r)
            in 
                if left >= right 
                then left + 1 
                else right + 1
            end;

fun rebalance(drevo: 'a bstree) : 'a bstree = 
    case drevo of
        lf => lf
        | br(left, value, right) =>
            let
                val leftHeight = height(left)
                val rightHeight = height(right)
                val balanceFactor = leftHeight - rightHeight
            in 
                if balanceFactor > 1 (*right rotation*)
                then 
                    (case left of 
                        lf => drevo
                        | br(lLeft, lValue, lRight) => if height(lLeft) >= height(lRight) 
                                                       then rotate(drevo, R)
                                                       else 
                                                            let
                                                                val leftLeft = rotate(left,L)
                                                                val combined = br(leftLeft,value,right)
                                                            in
                                                                rotate(combined, R)
                                                            end)
                                                            
                else if balanceFactor < (~1) 
                then 
                    (case right of 
                        lf => drevo
                        | br(rLeft, rValue, rRight) => if height(rRight) >= height(rLeft)
                                                       then rotate(drevo, L)
                                                       else 
                                                            let
                                                                val rightRight = rotate(right,R)
                                                                val combined = br(left, value, rightRight)
                                                            in
                                                                rotate(combined, L)
                                                            end)
                else drevo
                (* else br(left, value, right) *)
            end;



fun avl(c, drevo, e) = 
    case drevo of 
        lf => br(lf, e, lf)  
      | br(left, value, right) => 
            case c(e, value) of
                EQUAL => drevo  (*element ze notri*)
              | LESS => 
                    let
                        val newLeft = avl(c, left, e)
                        val newTree = br(newLeft, value, right)
                    in 
                        rebalance(newTree) 
                    end
              | GREATER => 
                    let
                        val newRight = avl(c, right, e)
                        val newTree = br(left, value, newRight)
                    in 
                        rebalance(newTree)
                    end;
