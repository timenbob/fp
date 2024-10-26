(* Test for neg *)
val _ = print "~~~~~~~~ neg ~~~~~~~~\n";
val test_type: number -> number = neg;
val test1 = neg(Succ(Succ(Zero))) = Pred(Pred(Zero));
val test2 = neg(Zero) = Zero;
val test3 = neg(Pred(Zero)) = Succ(Zero);

(* Test for add *)
val _ = print "~~~~~~~~ add ~~~~~~~~\n";
val test_type: number * number -> number = add;
val test1 = add(Succ(Succ(Zero)), Pred(Zero)) = Pred(Succ(Succ(Zero)));
val test2 = add(Zero, Zero) = Zero;
val test3 = add(Pred(Pred(Zero)), Succ(Succ(Zero))) = Zero;

(* Test for comp 
val _ = print "~~~~~~~~ comp ~~~~~~~~\n";
val test_type: number * number -> order = comp;
val test1 = comp(Succ(Succ(Zero)), Succ(Zero)) = GREATER;
val test2 = comp(Pred(Zero), Succ(Zero)) = LESS;
val test3 = comp(Zero, Zero) = EQUAL;*)

(* Test for contains *)
val _ = print "~~~~~~~~ contains ~~~~~~~~\n";
val test_type: tree * int -> bool = contains;
val test1 = contains(Node(3, Leaf(1), Leaf(4)), 4) = true;
val test2 = contains(Node(3, Leaf(1), Leaf(4)), 2) = false;
val test3 = contains(Leaf(5), 5) = true;

(* Test for countLeaves *)
val _ = print "~~~~~~~~ countLeaves ~~~~~~~~\n";
val test_type: tree -> int = countLeaves;
val test1 = countLeaves(Node(3, Leaf(1), Leaf(4))) = 2;
val test2 = countLeaves(Leaf(5)) = 1;
val test3 = countLeaves(Node(3, Node(1, Leaf(0), Leaf(2)), Leaf(4))) = 3;

(* Test for countBranches *)
val _ = print "~~~~~~~~ countBranches ~~~~~~~~\n";
val test_type: tree -> int = countBranches;
val test1 = countBranches(Node(3, Leaf(1), Leaf(4))) = 2;
val test2 = countBranches(Leaf(5)) = 0;
val test3 = countBranches(Node(3, Node(1, Leaf(0), Leaf(2)), Leaf(4))) = 4;

(* Test for height *)
val _ = print "~~~~~~~~ height ~~~~~~~~\n";
val test_type: tree -> int = height;
val test1 = height(Node(3, Leaf(1), Leaf(4))) = 2;
val test2 = height(Leaf(5)) = 1;
val test3 = height(Node(3, Node(1, Leaf(0), Leaf(2)), Leaf(4))) = 3;

(* Test for toList *)
val _ = print "~~~~~~~~ toList ~~~~~~~~\n";
val test_type: tree -> int list = toList;
val test1 = toList(Node(3, Leaf(1), Leaf(4))) = [1, 3, 4];
val test2 = toList(Leaf(5)) = [5];
val test3 = toList(Node(3, Node(1, Leaf(0), Leaf(2)), Leaf(4))) = [0, 1, 2, 3, 4];

(* Test for isBalanced *)
val _ = print "~~~~~~~~ isBalanced ~~~~~~~~\n";
val test_type: tree -> bool = isBalanced;
val test1 = isBalanced(Node(3, Leaf(1), Leaf(4))) = true;
val test2 = isBalanced(Node(3, Node(1, Leaf(0), Leaf(2)), Leaf(4))) = true;
val test3 = isBalanced(Node(3, Node(1, Leaf(0), Leaf(2)), Node(4, Leaf(5), Leaf(6)))) = true;
val test4 = isBalanced(Node(3, Node(1, Node(0,Leaf(2),Leaf(2)), Leaf(2)),Leaf(4))) = false;

(* Test for isBST *)
val _ = print "~~~~~~~~ isBST ~~~~~~~~\n";
val test_type: tree -> bool = isBST;
val test1 = isBST(Node(3, Leaf(1), Leaf(4))) = true;
val test2 = isBST(Node(3, Node(1, Leaf(0), Leaf(2)), Leaf(4))) = true;
val test3 = isBST(Node(3, Node(5, Leaf(1), Leaf(7)), Leaf(4))) = false;
