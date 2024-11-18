(* settings for long expressions *)
val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;
(* disable polyEq warnings *)
val _ = Control.polyEqWarn := false;


(* datatype for logical formulas *)
datatype 'a expression = 
    Not of 'a expression
|   Or of 'a expression list
|   And of 'a expression list
|   Eq of 'a expression list
|   Imp of 'a expression * 'a expression
|   Var of 'a
|   True | False;


(* linear congurence random number generator for function `prTestEq` *)
datatype 'a stream = Next of 'a * (unit -> 'a stream);

fun lcg seed =
    let fun lcg seed =
        Next (seed, fn () =>
            lcg (LargeInt.mod (1103515245 * seed + 12345, 0x7FFFFFFF)))
    in lcg (LargeInt.fromInt seed)
    end;

fun int2bool i = LargeInt.mod (i, 2) = 1;


(* conjutive normal form tester for function `satSolver` *)
fun isCNF (And es) =
    List.all
        (fn Or es => List.all (fn (Var _ | Not (Var _)) => true | _ => false) es
        |   (Var _ | Not (Var _)) => true
        |   _ => false) es
|   isCNF (Or es) = List.all (fn (Var _ | Not (Var _)) => true | _ => false) es
|   isCNF (True | False | Var _ | Not (Var _)) = true
|   isCNF _ = false;
(* exception for function `satSolver` *)
exception InvalidCNF;


(* ==================== SOME HELPER FUN. ==================== *)

(* operator for low priority right associative applications *)
infixr 1 $;
fun f $ x = f x;

(* curried equlity test *)
fun eq a b = a = b;

(* curried inequlity test *)
fun neq a b = a <> b;

(* removes all occurrences of `x` from a list *)
fun remove x = List.filter (neq x);

(* exception for nonimplemented functions *)
exception NotImplemented;


(* ==================== HELPER FUN. ==================== *)

infixr 1 $;
fun f $ x = f x;

fun eq a b = a = b;

fun neq a b = a <> b;

fun remove x = List.filter (neq x);


(* ==================== WARMUP ==================== *)

fun isolate (a : ''a list) : ''a list =
    if a=[] then [] 
    else
        let
        val b = rev a
        fun obrni [] = []
            | obrni (x::xs) = 
                if List.exists (fn y => y = x) xs then
                    obrni xs  (* preskoči trenutni element, če je že v seznamu *)
                else
                    x :: obrni xs;  (* dodaj element v seznam, če ga še ni bilo *)
        in
        rev(obrni(b))
        end;


(* ==================== PART 1 ==================== *)


fun getVars a =
    let
        (* Pomožna funkcija, ki rekurzivno prehodi izraz in zbere imena spremenljivk *)
        fun funP (Var x) = [x]
          | funP (Not e) = funP e
          | funP (Or es) = List.concat  (List.map funP es)
          | funP (And es) = List.concat (List.map funP es)
          | funP (Imp (e1, e2)) = funP e1 @ funP e2
          | funP (Eq es) = List.concat (List.map funP es)
          | funP _ = []  
    in
        isolate (funP a)  
    end;

fun eval(spremenljivke, funkcija) =
    let
        (* Pomožna funkcija, ki preveri, če je spremenljivka v seznamu *)
        fun resnica x = List.exists (fn y => y = x) spremenljivke
        
        (* Pomožna funkcija za oceno logičnih izrazov *)
        fun oceni True = true
          | oceni False = false
          | oceni (Var x) = resnica x
          | oceni (Not x) = not (oceni x)
          | oceni (Or xy) = List.exists (fn y => oceni y) xy
          | oceni (And xy) = List.all (fn y => oceni y) xy
          | oceni (Imp (e1, e2)) = (not (oceni e1)) orelse (oceni e2)
          | oceni (Eq es) =
              case es of
                  [] => true
                | h :: t => List.all (fn x => oceni x = oceni h) t
    in
        oceni funkcija
    end;


fun rmEmpty ex=
    case ex of
     Or []=> False
    | And []=> True
    | Eq [] => True                 
    | Or [x] => rmEmpty x              
    | And [x] => rmEmpty x           
    | Eq [x] => True                  
    | Or xs => Or (List.map rmEmpty xs)
    | And xs => And (List.map rmEmpty xs) (*z map naredimo da se rmEmpty da na vsak el notri*)
    | Eq xs => Eq (List.map rmEmpty xs)
    | Not x => Not (rmEmpty x)
    | Imp (e1, e2) => Imp (rmEmpty e1, rmEmpty e2)
    | _ => ex;    

              

fun pushNegations exs =
    let
        (* Function for pushing negations *)
        fun no e = pushNegations (Not e)
        
        (* Apply rmEmpty to remove trivial expressions like True/False *)
        val ex = rmEmpty exs

        (* Define allPairs function *)
        fun allPairs [] = []
        | allPairs (x::xs) = List.map (fn y => (x, y)) xs @ allPairs xs;

    in
    case ex of
        True => True
      | False => False
      | Var x => Var x
      | Not True => False
      | Not False => True
      | Not (Not e) => no e
      | Not (And e) => Or (List.map no e)
      | Not (Or e) => And (List.map no e)
      | Not (Imp (e1, e2)) => And [no e1, no e2]
      | Not (Eq e) => 
          let
            val pairs = allPairs e
            val negatedPairs = List.concat (List.map (fn (x, y) => [And [no x, no y], And [no x, no (Not y)]]) pairs)
          in
            Or negatedPairs
          end
      | And e => And (List.map no e)
      | Or e => Or (List.map no e)
      | Imp (e1, e2) => Or [no e1, no e2]
      | Eq es => 
          let
            val pairs = allPairs es
            val negatedPairs = List.concat (List.map (fn (x, y) => [Or [no x, no y], Or [no x, no (Not y)]]) pairs)
          in
            And negatedPairs
          end
      | _ => ex  (* Catch-all pattern for any case not explicitly matched *)
    end

(*dobil pomoc ker se neda resit vse z case*)
fun rmConstants izraz = 
    let 
        val poenostavljen_izraz = rmEmpty izraz
        fun simpl(And esez) = 
                let 
                    val poenostavljen = List.map simpl esez
                    val noben_True = List.filter (fn x => x <> True) poenostavljen
                in 
                    (*A in 0 = 0*)
                    (if List.exists (fn x => x = False) poenostavljen 
                    then False
                    else 
                        (case noben_True of
                            [] => True
                            | [el] => el
                            | _ => And noben_True))
                end
                        

            | simpl(Or esez) = 
                let 
                    val poenostavljen = List.map simpl esez
                    val noben_False = List.filter (fn x => x <> False) poenostavljen
                in 
                    (*A ali 1 = 1*)
                    (if List.exists (fn x => x = True) poenostavljen 
                    then True
                    else 
                        (case noben_False of
                            [] => False
                            | [el] => el
                            | _ => Or noben_False))
                end
            | simpl(Imp (e1,e2)) =
                let 
                    val levi_del = simpl e1
                    val desni_del = simpl e2
                in
                    (case (levi_del, desni_del) of
                        (_, False) => Not levi_del
                        | (_, True) => True
                        | (False, _) => True
                        | (True, _) => desni_del
                        | _ => Imp (levi_del, desni_del))
                end
            | simpl(Eq esez) = 
                let 
                    val poenostavljen = List.map simpl esez
                    val ima_True = List.exists (fn x => x = True) poenostavljen
                    val ima_False = List.exists (fn x => x = False) poenostavljen
                    val ni_True_ni_False = List.filter (fn x => x <> True andalso x <> False) poenostavljen
                in 
                    (case (ima_True, ima_False) of
                        (true, true) => False (*izraz ima in 0 in 1*)
                        | (true, false) => And ni_True_ni_False (*Eq[A,B,1,C] ~ A in B in C*)
                        | (false, true) => And (List.map (fn x => Not x) ni_True_ni_False) (*Eq[A,B,0,C] ~ ¬A in ¬B in ¬C*)
                        | _ => Eq ni_True_ni_False)
                end
            | simpl(Not izrazek) = 
                (case simpl izrazek of
                    True => False
                    | False => True
                    | poenostavljen_izrazek => Not poenostavljen_izrazek)
            | simpl el = el
    in
        simpl poenostavljen_izraz
    end;

fun rmVars ex = 
    case ex of
        And es => 
            let 
                val simplified = List.map rmVars es
                val unique = isolate simplified 
            in
                if List.exists (fn x => x = False) unique then False  
                else if List.exists (fn x => x = True) unique then
                    And (List.filter (fn x => x <> True) unique)  
                else And unique
            end
        | Or es =>
            let
                val simplified = List.map rmVars es
                val unique = isolate simplified  
            in
                if List.exists (fn x => x = True) unique then True
                else if List.exists (fn x => x = False) unique then
                    Or (List.filter (fn x => x <> False) unique)
                else Or unique
            end
        | Eq es =>
            let
                val simplified = List.map rmVars es
                val unique = isolate simplified 
            in
                if List.exists (fn x => x = True) unique then True 
                else if List.exists (fn x => x = False) unique then
                    False  
                else Eq unique
            end
        | Imp (e1, e2) =>
            let
                val simplified_e1 = rmVars e1
                val simplified_e2 = rmVars e2
            in
                if simplified_e1 = simplified_e2 then True  
                else Imp (simplified_e1, simplified_e2)
            end
        | Not e => Not (rmVars e) 
        | _ => ex  

fun simplify(ex)=
    let
        fun iterativnaPoenostavitev trenutno prejšnje =
            if trenutno = prejšnje then trenutno  
            else iterativnaPoenostavitev 
                    (odstraniSpremenljivke (potisniNegacije (odstraniKonstante trenutno))) trenutno
    in
        iterativnaPoenostavitev (odstraniSpremenljivke (potisniNegacije (odstraniKonstante izraz))) izraz
    end;


fun prTestEq _ _ _ = raise NotImplemented;

fun satSolver _ = raise NotImplemented;

(*  Za namene terstiranja drugega dela seminarske naloge odkomentiraj
    spodnjo kodo v primeru, da funkcije satSolver nisi implementiral.
    Pred oddajo odstrani nasledji dve vrstici kode!
    Deluje samo za izraze oblike `And [Or [...], Or [...], ....]`*)

use "external_sat_solver.sml";
val satSolver = external_sat_solver;



(* ==================== PART 2 ==================== *)

type timetable = {day : string, time: int, course: string} list;
type student = {studentID : int, curriculum : string list};


fun problemReduction _ _ _ = raise NotImplemented;

fun solutionRepresentation _ = raise NotImplemented;