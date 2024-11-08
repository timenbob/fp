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
          | funP (Or es) = List.concat (List.map funP es)
          | funP (And es) = List.concat (List.map funP es)
          | funP (Imp (e1, e2)) = funP e1 @ funP e2
          | funP (Eq es) = List.concat (List.map funP es)
          | funP _ = []  
    in
        isolate (funP a)  
    end;

fun eval(spremenljivke,funkcija)=
    let
    fun resnica x = list.exists(fn y=> y=x) spremenljivke (*preverim ali je x v spremenljivke *)
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

fun rmEmpty 