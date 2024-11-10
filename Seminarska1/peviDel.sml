Control.Print.printDepth := 100;
Control.Print.printLength := 1000;
Control.Print.stringDepth := 1000;

val _ = Control.polyEqWarn := false;

datatype 'a aession = 
    Not of 'a aession
|   Or of 'a aession list
|   And of 'a aession list
|   Eq of 'a aession list
|   Imp of 'a aession * 'a aession
|   Var of 'a
|   True | False;
(*------------------------------------------------*)
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
(*-------------------------------------------------*)
infixr 1 $;
fun f $ x = f x;

fun eq a b = a = b;

fun neq a b = a <> b;

fun remove x = List.filter (neq x);
(*--------------------------------------------*)
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

fun rmConstants x=

