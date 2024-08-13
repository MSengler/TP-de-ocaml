(* Structure pile, fifo, lifo, prioritaire *)

(* Fonction servant à inverser une liste *)
let rec revlist l = match l with
    |[] -> []
    |[x] -> [x]
    |hd::q -> (revlist q)@[hd];;

  
(* Création du type first in first out (fifo) *)
type fifo = { mutable  deb : int list ; mutable  fin : int list};;
let monfifo = {deb=[]; fin=[]};;

(* Ajout d'éléments *)
let addfifo fifo x = 
    fifo.deb <- x::fifo.deb;;
addfifo monfifo 3;; 
addfifo monfifo 5;; 
addfifo monfifo 8;;

(* Fonction vérifiant si la liste est vide *)
let isempty fifo = 
    fifo.deb = [] && fifo.fin = [];;

(* Fonction enlevant un élément du fifo *)
let rec defiler fifo = match fifo.deb,fifo.fin with
    |[],[] -> failwith "fifo vide"
    |_,hd::q -> fifo.fin <- q ; hd 
    |l, [] -> fifo.fin <- revlist fifo.deb; fifo.deb <- []; defiler fifo;; 


(* Création d'une pile avec des priorités *)
type pilePrio = { mutable  deb : int list ; mutable prio : int list; mutable  fin : int list};;
let monpilePrio = {deb=[]; prio =[]; fin=[]};;

(* Ajout d'éléments *)
let addprio pilePrio x p = 
    pilePrio.deb <- x::pilePrio.deb;
    pilePrio.prio <- p::pilePrio.prio;;

addprio monpilePrio 4 2;;
addprio monpilePrio 5 2;;
addprio monpilePrio 42 1;;
addprio monpilePrio 8 1;;
addprio monpilePrio 9 5;;

(* Fonction qui enlève le n-ème élèments d'une liste *)
let enleve l n =
    let i = ref 0 in
    let rec enleverec l n = match l,!i with
        |[],_ -> failwith "Indice out range"
        |hd::q, j when j = n -> hd, q
        |hd::q, j -> i := !i+1; let   (hd1,q1) = enleverec q n in hd1, hd::q1
    in enleverec l n;;

(* Fonction enlevant l'élèment le plus prioritaire de la pile *)
let rec defilerPrio pilePrio = match pilePrio.deb, pilePrio.fin with
    |[],[] -> failwith "pile vide"
    |_, hd::q -> pilePrio.fin <- q; hd
    |l, [] -> let (i,m) = maxlist pilePrio.prio in let (h,q) = enleve pilePrio.deb i and (h2,q2) = enleve pilePrio.prio i 
                in pilePrio.deb <- q; pilePrio.prio<-q2; pilePrio.fin <- h::pilePrio.fin; defilerPrio pilePrio;;


(* Ajout de la notion de capacité limité du fifo *)
type fifolimi = { mutable  deb : int list ; mutable  fin : int list; mutable nbelt : int ; capacite : int};;
let monfifolimi = {deb=[]; fin=[]; nbelt = 0; capacite = 5};;
let isempty fifolimi = 
    fifolimi.deb = [] && fifolimi.fin = [];;
let enfile fifolimi x = 
    if fifolimi.nbelt<fifolimi.capacite then (fifolimi.deb <- x::fifolimi.deb; fifolimi.nbelt <- (fifolimi.nbelt+1))
    else failwith "Out of Capacity";;

