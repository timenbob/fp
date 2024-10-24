(* Test for factorial *)
val _ = print "~~~~~~~~ factorial ~~~~~~~~\n";
val test_type: int -> int = factorial;
val test1 = factorial 5 = 120;
val test2 = factorial 0 = 1;
val test3 = factorial 1 = 1;

(* Test for power *)
val _ = print "~~~~~~~~ power ~~~~~~~~\n";
val test_type: int * int -> int = power;
val test1 = power(2, 3) = 8;
val test2 = power(5, 0) = 1;
val test3 = power(10, 2) = 100;

(* Test for gcd *)
val _ = print "~~~~~~~~ gcd ~~~~~~~~\n";
val test_type: int * int -> int = gcd;
val test1 = gcd(48, 18) = 6;
val test2 = gcd(17, 13) = 1;
val test3 = gcd(10, 5) = 5;

(* Test for len *)
val _ = print "~~~~~~~~ len ~~~~~~~~\n";
val test_type: int list -> int = len;
val test1 = len([1, 2, 3]) = 3;
val test2 = len([]) = 0;
val test3 = len([42]) = 1;

(* Test for last *)
val _ = print "~~~~~~~~ last ~~~~~~~~\n";
val test_type: int list -> int option = last;
val test1 = last([1, 2, 3]) = SOME 3;
val test2 = last([]) = NONE;
val test3 = last([42]) = SOME 42;

(* Test for nth *)
val _ = print "~~~~~~~~ nth ~~~~~~~~\n";
val test_type: int list * int -> int option = nth;
val test1 = nth([1, 2, 3], 1) = SOME 2;
val test2 = nth([1, 2, 3], 3) = NONE;
val test3 = nth([42], 0) = SOME 42;

(* Test for insert *)
val _ = print "~~~~~~~~ insert ~~~~~~~~\n";
val test_type: int list * int * int -> int list = insert;
val test1 = insert([1, 2, 3], 1, 42) = [1, 42, 2, 3];
val test2 = insert([], 0, 42) = [42];
val test3 = insert([1, 2], 2, 100) = [1, 2, 100];

(* Test for delete *)
val _ = print "~~~~~~~~ delete ~~~~~~~~\n";
val test_type: int list * int -> int list = delete;
val test1 = delete([1, 2, 3, 2], 2) = [1, 3];
val test2 = delete([42], 42) = [];
val test3 = delete([1, 1, 1], 1) = [];

(* Test for reverse *)
val _ = print "~~~~~~~~ reverse ~~~~~~~~\n";
val test_type: int list -> int list = reverse;
val test1 = reverse([1, 2, 3]) = [3, 2, 1];
val test2 = reverse([]) = [];
val test3 = reverse([42]) = [42];

(* Test for palindrome *)
val _ = print "~~~~~~~~ palindrome ~~~~~~~~\n";
val test_type: int list -> bool = palindrome;
val test1 = palindrome([1, 2, 1]) = true;
val test2 = palindrome([1, 2, 3]) = false;
val test3 = palindrome([]) = true;
