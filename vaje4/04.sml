(* Podan seznam xs agregira z začetno vrednostjo z in funkcijo f v vrednost f (f (f z s_1) s_2) s_3) ... *)
(* Aggregates xs with an initial value z and function f and returns f (f (f z s_1) s_2) s_3) ... 
fun reduce f z [] = z
  | reduce f z (h::t) = f h (reduce f z t);

val rec reduce = fn f => fn z => fn
    [] => z
  | h::t => f h (reduce f z t)*)
fun reduce _ z [] = z
   | reduce f z (x::xs) = reduce f (f z x) xs;
(* Vrne seznam, ki vsebuje kvadrate števil iz vhodnega seznama. Uporabite List.map. *)
(* Returns a list of squares of the numbers. Use List.map. 

*)

val squares = List.map(fn x=>x*x);


 (*fun sequaes list = List.map(fn x=>x*x) list;

 Vrne seznam, ki vsebuje vsa soda števila iz vhodnega seznama. Uporabite List.filter. *)
(* Returns a list that contains only even numbers from xs. Use List.filter. 
val onlyEven = List.filter(fn x => x mod 2 = 0)*)

fun onlyEven list=List.filter(fn x => x mod 2 = 0) list;
(*
 Vrne najboljši niz glede na funkcijo f (prvi arg.). Funkcija f primerja dva niza in vrne true, če je prvi niz boljši od drugega. Uporabite List.foldl. Najboljši niz v praznem seznamu je prazen niz. *)
(* Returns the best string according to the function f (first arg.). The function f compares two strings and returns true if the first string is better than the other. Use List.foldl. The best string in an empty list is an empty string. *)
(*val bestString = fn : (string * string -> bool) -> string list -> string*)

fun bestString f [] =""
    | bestString f [x]=x
    | bestString f (h1::h2::tl) = 
        if f(h1,h2) then bestString f (h1::tl)
        else bestString f (h2::tl)

(*val bestString = fn f => fn
    [] => ""
  | [x] => x
  | h1::h2::tl =>
      if f (h1, h2) then bestString f (h1::tl) (*ne mores klicat funkcije v funckiji*)
      else bestString f (h2::tl)

val rec bestString = fn f => fn (*rec pove da je rekurzivno in pol dela
    [] => ""
  | [x] => x
  | h1::h2::tl =>
      if f (h1, h2) then bestString f (h1::tl)
      else bestString f (h2::tl);
*)
 
(* Vrne leksikografsko največji niz. Uporabite bestString. *)
(* Returns the largest string according to alphabetical ordering. Use bestString. *)
(*val largestString = fn : string list -> string*)*)


fun largestString ll =
let
    fun f (s1, s2) = 
        case String.compare(s1, s2) of
            LESS => false
          | EQUAL => true
          | GREATER => true
in 
    bestString f ll
end
(*
val largestString =
    let
        val compare = fn (s1, s2) =>
            case String.compare (s1, s2) of
                LESS => false
              | _ => true
    in
        bestString compare
    end

 Vrne najdaljši niz. Uporabite bestString. *)
(* Returns the longest string. Use bestString. *)
(*val longestString = fn : string list -> string*)

fun longestString ll =
    let
        fun f (s1, s2) = String.size s1 >= String.size s2
    in
        bestString f ll
    end

(*val longestString =
    let
        val compare = fn (s1, s2) => String.size s1 >= String.size s2
    in
        bestString compare
    end

 Seznam uredi naraščajoče z algoritmom quicksort. Prvi argument je funkcija za primerjanje. *)
(* Sorts the list with quicksort. First argument is a compare function. *)
(*val quicksort = fn : ('a * 'a -> order) -> 'a list -> 'a list*)

fun quicksort f [] = []
  | quicksort f (glava::rep) =
    let
        val (m, v) = List.partition (fn x => f(x, glava) = LESS) rep
    in
        quicksort f m @ (glava :: quicksort f v)
    end
(*
val rec quicksort = fn f => fn
    [] => []
  | glava::rep =>
      let
          val (m, v) = List.partition (fn x => f (x, glava) = LESS) rep
      in
          quicksort f m @ (glava :: quicksort f v)
      end


 Vrne skalarni produkt dveh vektorjev. Uporabite List.foldl in ListPair.map. *)
(* Returns the scalar product of two vectors. Use List.foldl and ListPair.map. 
val dot = fn : int list -> int list -> int*)

fun dot vec1 vec2 =
    let
        val gg = ListPair.map (fn (x, y) => x * y) (vec1, vec2)
    in
        case gg of
            [] => 0
            | h::t => h + (dot vec1 t)
    end

(*val dot = fn vec1 => fn vec2 =>
    let
        val products = ListPair.map (fn (x, y) => x * y) (vec1, vec2)
    in
        List.foldr (op +) 0 products
    end


 Vrne transponirano matriko. Matrika je podana z vrstičnimi vektorji od zgoraj navzdol:
  [[1,2,3],[4,5,6],[7,8,9]] predstavlja matriko
   [ 1 2 3 ]
   [ 4 5 6 ]
   [ 7 8 9 ]
*)
(* Returns the transpose of m. The matrix m is given with row vectors from top to bottom:
  [[1,2,3],[4,5,6],[7,8,9]] represents the matrix
   [ 1 2 3 ]
   [ 4 5 6 ]
   [ 7 8 9 ]

val transpose = fn : 'a list list -> 'a list list*)

fun transpose [] = []
  | transpose (row::rows) =
      let
        val transposedRest = transpose rows
        fun addColumn [] [] = []
          | addColumn (r::rs) (t::ts) = (r::t) :: addColumn rs ts
      in
        addColumn row transposedRest
      end;


(*val transpose = fn m =>
    if m = [] then []
    else
        let
            val emptyCols = List.map (fn _ => []) (hd m)
        in
            List.foldr (fn (row, acc) =>
                ListPair.map (fn (x, y) => x :: y) (row, acc)) emptyCols m
        end*)

(* Zmnoži dve matriki. Uporabite dot in transpose. *)
(* Multiplies two matrices. Use dot and transpose. 
val multiply = fn : int list list -> int list list -> int list list*)

fun multiply m1 m2 =
    let
        val m2_transposed = transpose m2
    in
        List.map (fn row1 => List.map (fn row2 => dot row1 row2) m2_transposed) m1
    end

(*val multiply = fn m1 => fn m2 =>
    let
        val m2Transposed = transpose m2
    in
        List.map (fn row1 =>
            List.map (fn row2 => dot row1 row2) m2Transposed) m1
    end
 V podanem seznamu prešteje zaporedne enake elemente in vrne seznam parov (vrednost, število ponovitev). Podobno deluje UNIX-ovo orodje
   uniq -c. *)
(* Counts successive equal elements and returns a list of pairs (value, count). The unix tool uniq -c works similarly. 
val group = fn : ''a list -> (''a * int) list*)

fun group [] = []
  | group [x] = [(x, 1)]  
  | group (x::y::xs) =
      if x = y then
        let val rest = group (y::xs)
        in
          case rest of
            (v, n)::t => (v, n + 1) :: t
          | [] => [(x, 1)]  
        end
      else
        (x, 1) :: group (y::xs)  

(*val rec group = fn
    [] => []
  | [x] => [(x, 1)]
  | x::y::xs =>
      if x = y then
          let
              val rest = group (y::xs)
          in
              case rest of
                  (v, n)::t => (v, n + 1) :: t
                | [] => [(x, 1)]
          end
      else
          (x, 1) :: group (y::xs)

 Elemente iz podanega seznama razvrsti v ekvivalenčne razrede. Znotraj razredov naj bodo elementi v istem vrstnem redu kot v podanem seznamu. Ekvivalentnost elementov definira funkcija f, ki za dva elementa vrne true, če sta ekvivalentna. *)
(* Sorts the elements from a list into equivalence classes. The order of elements inside each equivalence class should be the same as in the original list. The equivalence relation is given with a function f, which returns true, if two elements are equivalent. *)
(*val equivalenceClasses = fn : ('a -> 'a -> bool) -> 'a list -> 'a list list*)
fun equivalenceClasses _ [] = []
  | equivalenceClasses f (x::xs) =
      let
        val (group, rest) = List.partition (fn y => f(x, y)) xs
      in
        (x::group) :: equivalenceClasses f rest  
      end

(*val rec equivalenceClasses = fn f => fn
    [] => []
  | x::xs =>
      let
          val (group, rest) = List.partition (fn y => f (x, y)) xs
      in
          (x::group) :: equivalenceClasses f rest
      end
*)
