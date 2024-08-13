(* Logique *) 
type logique =
    True
    |False
    |And of logique*logique
    |Or of logique*logique
    |Not of logique 
    |Imp of logique*logique
    |Eq of logique*logique
    ;;

let myform = Not(Or(And(True,False),True));;
let myform = And(And(Not(Or(True, True)), Or(True,And(True,False))), And(True,False));;
let myform = Eq(False, False);;

let rec reduireFormule form = match form with
    |True -> True
    |False -> False
    |And(True, True) -> True
    |And(True, False) -> False
    |And(False, True) -> False
    |And(False, False) -> False
    |Or(True, True) -> True
    |Or(True, False) -> True
    |Or(False, True) -> True
    |Or(False, False) -> False
    |Not(True) -> False
    |Not(False) -> True
    |Imp(p, q) -> reduireFormule (Or(reduireFormule p, Not(reduireFormule q)))
    |Eq(p,q) ->reduireFormule (Or(And(reduireFormule p, reduireFormule q),And(Not(reduireFormule p), Not(reduireFormule q))))
    |And(p, q) -> reduireFormule (And(reduireFormule p, reduireFormule q))
    |Or(p, q) -> reduireFormule (Or(reduireFormule p, reduireFormule q))
    |Not(f) -> if reduireFormule f = True then False else True;;
