(* Arbre Binaire de Recherche *)

(* Création de notre type arbre et d'un objet arbre *)
type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;;
let monarbre = Node(5, Node(3, Empty, Empty), Empty);;

(* Fonction qui insert un élément dans notre arbre de façcon que ce soit toujours un ABR *)  		
let rec insert x btree = match btree with
    |Empty -> Node(x, Empty, Empty)
    | Node(y, left, right) ->
        if x <= y then Node(y, insert x left, right)
        else Node(y, left, insert x right);;
        
let monarbre = insert 2 monarbre;;
let monarbre = insert 6 monarbre;;
let monarbre = insert 2 monarbre;;


(* Cherche si l'élément x est dans l'arbre ou non *)
let rec trouverarbre x btree = match btree with
    |Empty -> false    
    |Node(y,bg, bd) -> if x==y then true   
        else if x<y then trouverarbre x bg  
        else trouverarbre x bd;;

        
(* Fonction donnant le miroir d'un arbre *)
let rec miroir btree = match btree with
    |Empty -> Empty
    |Node(x,bg,bd) -> Node(x, miroir bd, miroir bg);;

    
(* Fonction calculant la somme des noeuds de l'arbre *)
let rec somme btree = match btree with 
    |Empty -> 0
    |Node(x,bg,bd)-> x + somme bg + somme bd;;

(* Fonction calculant la hauteur maximal de l'arbre *)   
let rec hauteur btree = match btree with
    |Empty -> 0
    |Node(x,bg,bd) -> 1 + max (hauteur bg) (hauteur bd);;

(* Fonction calculant le nombre de feuilles *)
let rec nbleave btree = match btree with
    |Empty -> 0
    |Node(x,bg,bd) -> match bg,bd with
        |Empty,Empty -> 1
        |Empty,_ -> nbleave bd
        |_,Empty -> nbleave bg
        |_,_ -> nbleave bg + nbleave bd;;

(* Fonction calculant le nombre de noeuds *)
let rec nbnoeuds btree = match btree with
    |Empty -> 0
    |Node(x,bg,bd) -> 1 +nbnoeuds bg + nbnoeuds bd;;

(* Fonction calculant le nombre de noeuds intérieurs*)
let rec nbnoeudsinterne btree = match btree with
    |Empty -> 0
    |Node(_,Empty,Empty) -> 0
    |Node(x,bg,bd) -> 1 +nbnoeudsinterne bg + nbnoeudsinterne bd;;

(* Fonction parcourant l'arbre en profondeur infixe *)
let rec parcourir btree = match btree with
    |Empty -> ""
    |Node(x,bg,bd) -> parcourir bg ^" "^string_of_int(x)^" "^parcourir bd;;

(* Fonction parcourant les feuille de l'arbre *)
let rec parcourirfeuille btree = match btree with
    |Empty -> []
    |Node(x,Empty,Empty) -> [x]
    |Node(x,bg,bd) -> parcourirfeuille bg @ parcourirfeuille bd;;

(* Fonction parcourant l'arbre en largeur. On utilise le type fifo pour simplifier la fonction *)
type 'a fifoarbre = { mutable  deb : 'a btree list ; mutable  fin : 'a btree list};;
let enfile fifo x = 
    fifo.deb <- x::fifo.deb;;
let isempty fifo = 
    fifo.deb = [] && fifo.fin = [];;
let rec defiler fifo = match fifo.deb,fifo.fin with
    |[],[] -> failwith "fifo vide"
    |_,hd::q -> fifo.fin <- q ; hd 
    |l, [] -> fifo.fin <- revlist fifo.deb; fifo.deb <- []; defiler fifo;; 
    
let parcourirLargeur btree = match btree with
    |Empty -> []
    |Node(x,Empty,Empty) -> [x]
    |Node(x,bg,bd) -> let maqueue = {deb=[Node(x,bg,bd)]; fin=[]} and visiter = ref [] in
        while isempty maqueue = false do 
            let monssarbre = defiler maqueue in 
            let mynode tree = match tree with |Empty -> failwith "liste vide" |Node((x,bg,bd))-> x,bg,bd
            in let node, bg, bd = mynode monssarbre  in 
            visiter := node::!visiter;
            if bg <> Empty then enfile maqueue bg;
            if bd <> Empty then enfile maqueue bd;
        done; revlist !visiter;;
    



(* Fonction donnant le maximum de l'arbre *)
let rec maxArbre btree = match btree with
    |Empty -> invalid_arg "Unexpected case encountered in process_value_invalid_arg"
  	|Node(x, Empty, Empty) -> x
  	|Node(x, bg, Empty) -> x
  	|Node(x, bg, bd) -> maxArbre bd;;

(* Fonctions enlevant le maximum *)
let rec enleveMax btree = match btree with
  	|Empty -> Empty
  	|Node(x, Empty, Empty) -> Empty
  	|Node(x, Node(x2,bg,bd),Empty) -> Node(x2, bg,bd)
  	|Node(x,bg,bd) -> Node(x, bg, enleveMax bd);;

let rec enleveMax2 btree = match btree with
    |Empty -> failwith "Impossible case"
    |Node(m,bg,Empty) -> m,bg
    |Node(x,bg,bd) -> let (md,hd) = enleveMax2 bd in (md, Node(x,bg, hd));;
    
(* Fonctions permettant de retirer x et reajuster l'arbre pour qu'il reste un ABR *)
let rec retirer x btree = match btree with
  	|Empty -> Empty
  	|Node(y,bg,bd) -> if x != y then Node(y, retirer x bg, retirer x bd)
      		else match bg,bd with
          |Empty, Empty -> Empty
          |Empty, Node(y2,bg2,bd2) -> Node(y2,bg2,bd2)
          |Node(y2,bg2,bd2), Empty -> Node(y2, bg2, bd2)
          |_, _ -> Node(maxArbre bg, enleveMax bg, bd);;

let rec enleve x btree = match btree with
    |Empty -> Empty
    |Node(y,bg,bd) when x<y -> Node(y, enleve x bg, bd )
    |Node(y,bg,bd) when x>y -> Node(y, bg, enleve x bd )
    |Node(y,bg,bd) -> let (md,hd) = enleveMax2 bg in Node(md,hd,bd);;
