(* Liste *)

(* Fonction qui renvoie true si un élémént x est présent dans la liste *)
let rec trouverlist l x = match l with
    |[] -> false
    |hd::q -> if hd == x then true 
        else trouverlist q x;;

(* Fonction qui ajoute un x dans une liste ordonnée *)
let rec addlist l x = match l with
    |[] -> [x]
    |hd::q -> if x<=hd then x::l
            else hd::(addlist q x);;

(* Fonction qui supprime un élément x de la liste si il est présent *)
let rec dellist l x = match l with
    |[] -> []
    |hd::q -> if hd==x then q
            else hd::dellist q x;;

(* Fonction qui supprime tout les éléments x présents dans la liste *)
let rec delalllist l x = match l with
    |[] -> []
    |hd::q -> if hd==x then delalllist q x
            else hd::delalllist q x;;

(* Fonction qui inverse l'ordre des éléments dans la liste *) 
let rec revlist l = match l with
    |[] -> []
    |[x] -> [x]
    |hd::q -> (revlist q)@[hd];;

(* Fonction vérifiant que le début d'un liste corresponde à une liste plus petite *)
let rec egalsublist l leg = match l, leg with
    |_,[] -> true
    |[],_ -> false
    |hd::q, hds::qs -> if hd != hds then false
                    else egalsublist q qs;;

                    
(* Fonction détectant si une liste est une sous liste d'une autre liste *)
let rec sublist l lsub = match l, lsub with
    |[],_ -> false
    |_,[] -> false
    |hd::q,hds::qs -> if hd != hds then sublist q lsub
                    else egalsublist q qs || sublist q lsub;;

(* Fonction donnant le maximum et son indice d'une liste *)
let maxlist l = 
    let i = ref 0 in match l with
    |[] -> invalid_arg "Unexpected case encountered in process_value_invalid_arg"
    |[x] -> !i,x
    |m::q ->let j = ref 0 in let rec maxlistrec l m =  match l with
                |[] -> !i,m 
                |hd::q ->  if hd>m then (i:=!j;j := !j+1; maxlistrec q hd)
                        else (j := !j+1; maxlistrec q m)
            in maxlistrec l m;;


(* Fonction vérifiant si une liste est triée ou non *)
let rec sortedlist l = match l with
    |[] -> true
    |[x] -> true
    |hd::hd2::q -> if hd <= hd2 then sortedlist (hd2::q)
                    else false;;

(* Fonctions triant la liste *)
let rec sortlist l = match l with
    | [] -> []
    | hd :: q -> addlist (sortlist q) hd;;

(* Fonction appliquant une fonction aux éléments d'une liste *)
let rec map f l = match l with
    | [] -> []
    | hd :: q -> f hd :: map f q;;
    
let rec maplist l f = match l with
    |[] -> []
    |hd::q ->(f hd)::maplist q f;;
    
(* Fonction faisant le produit scalaire d'un liste *)
let rec pdscal l1 l2  = match l1, l2 with
    |[],[] -> 0.
    |[],_ -> invalid_arg "Unexpected case encountered in process_value_invalid_arg"
    |_,[] -> invalid_arg "Unexpected case encountered in process_value_invalid_arg"
    |hd::q, hd2::q2 -> hd*.hd2 +. pdscal q q2;;

(* Fonction calculant la longueur d'une liste *)
let rec longueurlist l = match l with 
    |[] -> 0
    |hd::q -> 1+longueurlist q;;

(* Fonction faisant la concaténation de deux listes *)
let rec concat l1 l2 = match l1, l2 with
    |l,[] -> l 
    |[],l -> l 
    |hd::q, _ -> hd::concat q l2;;


(* Fonctions pour faire le tri fusion d'une liste *)
let rec fusion l1 l2 = match l1, l2 with
    |[],[] -> []
    |[],l2 -> l2
    |l1,[] -> l1
    |hd1::q1,hd2::q2 -> if hd1<hd2 then hd1::fusion q1 l2
                    else hd2::fusion l1 q2;;

let rec diviselist l m n = match l with
    |[] -> [],[]
    |hd::q -> if m<n then let (l1,l2) = (diviselist q (m+1) n) in hd::l1,l2
            else let (l1,l2) = (diviselist q (m+1) n) in l1, hd::l2;;

let rec tri_fusion l =
    let n = longueurlist l in match n with
        |0 -> []
        | 1 -> let l1,l2 = diviselist l 0 1 in fusion l1 l2
        | 2 -> let l1,l2 = diviselist l 0 1 in fusion l1 l2
        | _ -> let l1,l2 = diviselist l 0 (n/2) in fusion (tri_fusion l1) (tri_fusion l2);;
