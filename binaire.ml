(* Binaire *)

let addbinary l1 l2 = 
    let l1 = revlist l1 and l2 = revlist l2 in 
    let rec addbinaryrec l1 l2 = 
        match l1, l2 with 
        |[],[] -> []
        |l1,[] -> l1
        |[], l2 -> l2
        |0::q1, 0::q2 -> 0::(addbinaryrec q1 q2)
        |1::q1, 0::q2 -> 1::(addbinaryrec q1 q2)
        |0::q1, 1::q2 -> 1::(addbinaryrec q1 q2) 
        |1::q1, 1::q2 -> 0::(addbinaryrec (addbinaryrec q1 [1]) q2)
        |_, _ -> failwith "Liste non binaire"
    in let lb = addbinaryrec l1 l2 in 
        revlist lb;;

let readbinary l =
    let n = 0 and lrev = revlist l in 
    let rec readbinaryrec l n = match l with
        |[] -> 0
        |h::q when h=0 -> readbinaryrec q (n+1)
        |h::q when h=1 -> int_of_float(2.**float_of_int(n))+readbinaryrec q (n+1)
        |_ -> failwith "Liste non binaire"
    in readbinaryrec lrev n;;

let tobinary n = 
    let rec tobinaryrec n = match n with
        |0 -> []
        |1 -> [1]
        |n when n mod 2 = 0 -> 0::tobinaryrec (n/2)
        |n when n mod 2 = 1 -> 1::tobinaryrec (n/2)
        |_-> failwith "n non valable"
    in let l = tobinaryrec n in revlist l ;;
