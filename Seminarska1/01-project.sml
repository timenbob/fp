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

fun isolate l = 
    let
        fun pomozna([],_) = []
            | pomozna(glava::rep, videni) = if List.exists (fn y => y = glava) videni
                                           (*ce je glava ze v videnih preskoci*)
                                           then pomozna(rep, videni)
                                           (*dodamo element v seznam*)
                                           else glava::pomozna(rep, glava::videni)
    in 
        pomozna(l,[])
    end;
(* ==================== PART 1 ==================== *)

fun getVars (a) =
    let
        (* Pomožna funkcija, ki rekurzivno prehodi izraz in zbere imena spremenljivk *)
        fun funP (Var x) = [x](*samo dodamo notri*)
          | funP (Not e) = funP e(*poklicemo na izrazu*)
          | funP (Or es) = List.concat  (List.map funP es)
          | funP (And es) = List.concat (List.map funP es)(*map najprej na vsak el poklice fun [[r1], [r2], [r3]], nato concat zdruzi*)
          | funP (Imp (e1, e2)) = funP e1 @ funP e2(*samo zdruzi*)
          | funP (Eq es) = List.concat (List.map funP es)
          | funP _ = []  
    in
        isolate (funP a)  
    end;

fun eval spremenljivke funkcija=
    let
        fun fune(Var x) = List.exists (fn y => y = x) spremenljivke (*pogledamo a je x v spremenljivke*)
            | fune(Not x) = not (fune x)
            | fune(Or el) = List.exists fune el (*vrne true ce je kksn el true*)
            | fune(And el) = List.all fune el (*vsi morjo bit ok*)
            | fune(Eq el) = 
                (case el of 
                    [] => true(*po navodilih*)
                    | [el] => true(*ce en el true*)
                    | glava::rep => let 
                                        val eg = fune glava(*ce se ujema z glavo je gud*)
                                    in
                                        List.all (fn el => eg = fune el) rep (*preverimo ali so vsi ostali elementi enaki kot prvi*)
                                    end)
            | fune(Imp (e1, e2)) = (not (fune e1)) orelse (fune e2)(*obinci lmn*)
            | fune(True) = true
            | fune(False) = false
    in
        fune funkcija
    end;



fun rmEmpty ex=
    case ex of
        Or []=> False
        | And []=> True
        | Eq [] => True(*te pogledu na net da majo smisu*)                 
        | Or [x] => rmEmpty x              
        | And [x] => rmEmpty x           
        | Eq [x] => True  (*posamezni elemeti*)                
        | Or xs => Or (List.map rmEmpty xs)
        | And xs => And (List.map rmEmpty xs) (*z map naredimo da se rmEmpty da na vsak el notri*)
        | Eq xs => Eq (List.map rmEmpty xs)
        | Not x => Not (rmEmpty x)
        | Imp (e1, e2) => Imp (rmEmpty e1, rmEmpty e2)(*implikacijo pustimo samo te notri postimamo ce so or and al eq*)
        | _ => ex;    

              

fun pushNegations exs=
    let
       
        val ex = rmEmpty exs(*navodila*)
        fun pomoc(Not(Not(el)))= pomoc el
            | pomoc(Not (And el)) = Or (List.map (fn x => pomoc(Not x)) el)
            | pomoc(Not (Or el)) = And (List.map (fn x => pomoc(Not x)) el)(*obicen lmn*)
            | pomoc(Not (Imp (e1,e2))) = And [pomoc e1, pomoc(Not e2)](*obicen lmn*)

            | pomoc(Not (Eq el)) = And [Or (List.map (fn e => pomoc (Not e)) el), Or (List.map pomoc el)]
            (*una glupa ogabna formula*)

            | pomoc(And el) = And (List.map pomoc el)
            | pomoc(Or el) = Or (List.map pomoc el)
            | pomoc(Imp (e1,e2)) = Imp (pomoc e1, pomoc e2)
            | pomoc(Eq el) = Eq (List.map pomoc el)
            | pomoc(Not el) = Not (pomoc el)(*samo da une not postimas*)
            | pomoc (el)=el(*ko nic drugega naceloma sem nebi smelo nic prit*)
    in
        pomoc ex
    end

fun rmConstants exp =
    let
        val ex = rmEmpty exp;

        fun neg True = False
        |   neg False = True
        |   neg e = Not e;(*navodil*)


        fun pomoc ex = 
            case (rmEmpty ex) of
                Not e => neg (pomoc e)(*samoo neg*)
              | Or l =>
                    let
                        val r = remove False (List.map pomoc l)
                    in
                        if List.exists (eq True) r then True else Or r
                    end
              | And l =>
                    let
                        val r = remove True (List.map pomoc l)(*truje damo vn ker nimajo veze*)
                    in
                        if List.exists (eq False) r then False else And r(*ce je vsaj ena false bam stran*)
                    end
              | Imp (e1, e2) => 
                    let
                        val (e1', e2') = (pomoc e1, pomoc e2)
                    in
                        case (e1', e2') of
                            (e, False) => pomoc (Not e)
                          | (e, True) => True
                          | (False, _) => True
                          | (True, e) => pomoc e(*iz tabele*)
                          | (el, er) => Imp (el, er)(*ostale moznosti*)
                    end
              | Eq l =>
                    let
                        val r = List.map pomoc l
                        val (hasTrue, hasFalse) = (List.exists (eq True) r, List.exists (eq False) r)(*preverimo a mamo truje pa false*)
                    in
                        if hasTrue andalso hasFalse then
                            False
                        else if hasTrue then
                            And (remove True r)(*odstranmo vse truje*)
                        else if hasFalse then
                            And (List.map neg (remove False r))(*odstranmo vse folse in negiramo in zdruzimo z and*)
                        else
                            Eq r
                    end
              | e => e
    in
        rmEmpty (pomoc ex)(*ce smo spremenil kksne*)
    end;

fun rmVars ex = 
    let
        val poenostavljeno = rmEmpty ex
        fun duplikatiStran a=isolate a(*dobimo list nazaj*)

        fun poenostavi ex=
            case ex of
                And es => 
                    let 
                        val a=duplikatiStran(List.map poenostavi es)(*ostanejo le razlicni*)
                    in
                        case a of
                            []=> True
                            |[el]=>el
                            |_=> And a(*samo logika lmn shit*)
                    end
                | Or es =>
                    let
                        val a=duplikatiStran(List.map poenostavi es)
                    in
                        case a of
                            []=> False
                            |[el]=>el
                            |_=> Or a(*lmn shit*)
                    end
                | Eq es =>
                    let
                        val a=duplikatiStran(List.map poenostavi es)
                    in
                        case a of
                            []=> False
                            |[_]=>True
                            |_=> Eq a
                    end
                | Imp (e1, e2) =>
                    let
                        val simplified_e1 = rmVars e1(*Poberemo vn stvari*)
                        val simplified_e2 = rmVars e2
                    in
                        if simplified_e1 = simplified_e2 then True  
                        else Imp (simplified_e1, simplified_e2)(*lmn shit*)
                    end
                | Not e => Not (rmVars e) 
                | _ => ex  

        in
            poenostavi ex
        end

fun simplify izraz =
    let
        fun poenostavljeno sez=rmVars( pushNegations( rmConstants( sez))) (*zaporedje delas dokler ne dela*)
        fun poenostavi el=
            let 
               val pon_el=poenostavljeno el(*smo poenostavili*)
            in
                if pon_el = el then el (*dokler se kej spremeni deli*)
                else poenostavi pon_el
            end
        
    in
        poenostavi izraz
    end;

datatype 'a stream = Next of 'a * (unit -> 'a stream);

fun lcg seed =
    let fun lcg seed =
        Next (seed, fn () =>
            lcg (LargeInt.mod (1103515245 * seed + 12345, 0x7FFFFFFF)))
    in lcg (LargeInt.fromInt seed)
    end;

fun int2bool i = LargeInt.mod (i, 2) = 1;


fun prTestEq seed ex1 ex2 = (*ni ok neki manka*)
    let
        val exV1 = getVars ex1
        val exV2 = getVars ex2
        val vse_vr = isolate (exV1 @ exV2)
        val Next (_, next_one) = lcg seed 

        fun get_bool (0, _) = []  
          | get_bool (n, Next (i, next)) = int2bool i :: get_bool (n - 1, next ())

        val bolean = get_bool (List.length vse_vr, next_one ())
        val sez = ListPair.zip (vse_vr, bolean)
        val spr = List.map #1 (List.filter (fn (_, b) => b) sez)
        val rez1 = eval spr ex1
        val rez2 = eval spr ex2
    in 
        rez1 = rez2 
    end;

fun satSolver _ = raise NotImplemented;

(*  Za namene terstiranja drugega dela seminarske naloge odkomentiraj
    spodnjo kodo v primeru, da funkcije satSolver nisi implementiral.
    Pred oddajo odstrani nasledji dve vrstici kode!
    Deluje samo za izraze oblike `And [Or [...], Or [...], ....]`*)

(* use "external_sat_solver.sml";
val satSolver = external_sat_solver;
 *)


(* ==================== PART 2 ==================== *)

type timetable = {day : string, time: int, course: string} list;
type student = {studentID : int, curriculum : string list};


fun problemReduction _ _ _ = raise NotImplemented;

fun solutionRepresentation _ = raise NotImplemented; 