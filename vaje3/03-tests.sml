(* Test for zip *)
val _ = print "~~~~~~~~ zip ~~~~~~~~\n";
val test_type: ('a list * 'b list) -> ('a * 'b) list = zip;
val zipTest1 = zip ([1, 2, 3], ["a", "b", "c"]) = [(1, "a"), (2, "b"), (3, "c")];
val zipTest2 = zip ([1, 2], ["a", "b", "c"]) = [(1, "a"), (2, "b")];
val zipTest3 = zip ([1, 2, 3], ["a"]) = [(1, "a")];
val zipTest4 = zip ([], []) = [];
val zipTest5 = zip ([1, 2, 3], []) = [];

(* Test for unzip *)
val _ = print "~~~~~~~~ unzip ~~~~~~~~\n";
val test_type: ('a * 'b) list -> 'a list * 'b list = unzip;
val unzipTest1 = unzip [(1, "a"), (2, "b"), (3, "c")] = ([1, 2, 3], ["a", "b", "c"]);
val unzipTest2 = unzip [] = ([], []);

(*
(* Test for subtract *)
    val _ = print "~~~~~~~~ subtract ~~~~~~~~\n";
    val test_type: natural * natural -> natural = subtract;

    (* Test 1: This expects a NotNaturalNumber exception with value One *)
    val subtractTest1 = (subtract (One, Succ One) handle NotNaturalNumber n => n = One);

    (* Test 2: Normal result, expecting no exception *)
    val subtractTest2 = (subtract (Succ(Succ One), One) = Succ One);

    (* Test 3: Normal result, expecting no exception *)
    val subtractTest3 = (subtract (Succ(Succ(Succ One)), Succ(Succ One)) = Succ One);

*)



(* Test for any *)
val _ = print "~~~~~~~~ any ~~~~~~~~\n";
val test_type: ('a -> bool) * 'a list -> bool = any;
val anyTest1 = any ((fn x => x > 0), [1, 2, 3]) = true;
val anyTest2 = any ((fn x => x > 3), [1, 2, 3]) = false;
val anyTest3 = any ((fn x => x > 0), []) = false;

(* Test for map *)
val _ = print "~~~~~~~~ map ~~~~~~~~\n";
val test_type: ('a -> 'b) * 'a list -> 'b list = map;
val mapTest1 = map ((fn x => x + 1), [1, 2, 3]) = [2, 3, 4];
val mapTest2 = map (fn x => x * x, [1, 2, 3, 4]) = [1, 4, 9, 16];
val mapTest3 = map (fn x => x + 1, []) = [];

(* Test for filter *)
val _ = print "~~~~~~~~ filter ~~~~~~~~\n";
val test_type: ('a -> bool) * 'a list -> 'a list = filter;
val filterTest1 = filter ((fn x => x > 1), [1, 2, 3]) = [2, 3];
val filterTest2 = filter ((fn x => x mod 2 = 0), [1, 2, 3, 4, 5]) = [2, 4];
val filterTest3 = filter ((fn x => x < 0), []) = [];

(* Test for fold *)
val _ = print "~~~~~~~~ fold ~~~~~~~~\n";
val test_type: ('a * 'b -> 'a) * 'a * 'b list -> 'a = fold;
val foldTest1 = fold (op +, 0, [1, 2, 3, 4]) = 10;
val foldTest2 = fold (op *, 1, [1, 2, 3, 4]) = 24;
val foldTest3 = fold (op ^, "", ["a", "b", "c"]) = "abc";

(* Test for rotate *)
val _ = print "~~~~~~~~ rotate ~~~~~~~~\n";
val test_type: 'a bstree * direction -> 'a bstree = rotate;
val rotateTest1 = rotate (br (lf, 10, br (lf, 20, lf)), L) = br (lf, 20, br (lf, 10, lf));
val rotateTest2 = rotate (br (br (lf, 10, lf), 20, lf), R) = br (br (lf, 10, lf), 20, lf);

(* Test for rebalance *)
val _ = print "~~~~~~~~ rebalance ~~~~~~~~\n";
val test_type: 'a bstree -> 'a bstree = rebalance;
val rebalanceTest1 = rebalance (br (br (lf, 10, lf), 20, br (lf, 30, br (lf, 40, lf)))) = br (br (lf, 10, lf), 20, br (lf, 30, br (lf, 40, lf)));

(* Test for avl *)
val _ = print "~~~~~~~~ avl ~~~~~~~~\n";
val test_type: ('a * 'a -> order) * 'a bstree * 'a -> 'a bstree = avl;
val avlTest1 = avl (c, lf, 10) = br (lf, 10, lf);
val avlTest2 = avl (c , br (lf, 20, lf), 10) = br (br (lf, 10, lf), 20, lf);
val avlTest3 = avl (c, br (lf, 10, lf), 20) = br (lf, 10, br (lf, 20, lf));



