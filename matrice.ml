(* Matrice *)
include Array;;
let m = [|[| 1; 2; 3; 4 |]; [| 4; 5; 6; 7 |]; [| 7; 8; 9; 10 |] |];;
let n = [|[| 1; 1; 1; 1 |]; [| 1; 1; 1; 1 |]; [| 1; 1; 1; 1 |] |];;
m.(0).(0) <- 5;;

let add a b = a+b;;let add a b = a+.b;;
let mult a b = a*b;;let add a b = a*.b;;
let sub a b = a-b;; let sub ab = a-.b;;
let div a b = if b = 0. then failwith "division par zéros" else a /. b;;

let operation f m n = 
    if length m <> length n  then failwith " Matrix have not the same shape "
    else let new_m = make_matrix (length m) (length m.(0)) 0 in 
        for i = 0 to (length m - 1) do 
        if length m.(i) <> length n.(i)  then failwith " Matrix have not the same shape "
        else for j=0 to (length m.(i) - 1) do 
            new_m.(i).(j) <-  f m.(i).(j) n.(i).(j); 
            done; 
        done; 
        new_m;;

let map f m = 
    let new_m = make_matrix (length m) (length m.(0)) 0 in
    for i = 0 to (length m - 1) do for j=0 to (length m.(i) - 1) do
        new_m.(i).(j) <- f m.(i).(j)
    done; done; new_m;;
