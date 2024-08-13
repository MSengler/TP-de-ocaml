(* Fonction / Suite / Arithmetique *)
let absf x = 
    if x < 0. then -.x
    else x;;
  
let derivate f x =
    let h = 1e-10 in
    (f(x+.h)-.f(x)) /. h;;
let derivate f x =
    let h = 1e-10 in
    (f(x+.h)-. f(x -. h)) /. (2. *. h) ;;

let derivate2 f x = 
    let h = 1e-5 in 
    (f(x+.h) -. 2.*.f(x) +. f(x-.h)) /. h**2. ;;

let f x = x ** 2.;;


let heron a =
    let rec heronrec a u = 
        let u1 = (u +. a /. u) /. 2. in 
        if absf (u -. u1) < 1e-3 then u1
        else heronrec a u1
    in heronrec a 1.;;


let rec dichotomie a b eps f =
    if (b-.a) < eps then a
    else  
        let c = (a+.b) /. 2. in
        if f(c) <= 0. then dichotomie c b eps f
        else dichotomie a c eps f;;

let pointfixe f = 
    let k = 100 and i = ref 0 and x = ref 1. in 
    while !i<k do i := !i+1; x := f (!x) ; done;
    !x;;

let rec pgcd p q = match p,q with
    |p,_ when p <= 0 -> failwith "valeur de p négatif"
    |_,q when q < 0 -> failwith "valeur de q négatif"
    |p,0 -> p
    |p,q -> pgcd q (p mod q);;

let allcommundivisor n = 
    let l = ref [] in
    for i=1 to n do 
        let p = pgcd i n in 
        if p !=1 then l := i::(!l)
    done;
    l;;

let alldivisor n = 
    let l = ref [] in
    for i=1 to n do 
        if n mod i = 0 then l := i::(!l)
    done;
    l;;

