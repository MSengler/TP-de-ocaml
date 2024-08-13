(* Dictionnaire : usage dans le cas d'une classe *)

type eleve = {nom : string; prenom: string; mutable age : int; mutable moygal : float};;
let enaelle = {nom="Flaubert"; prenom="Enaelle"; age=17; moygal = 11.5};;
let louis = {nom="Smith"; prenom="Louis"; age=16; moygal = 13.7};;
let lucien = {nom="Poitou"; prenom="Lucien"; age=17; moygal=15.6};;

let maclass = ref [];;
maclass := enaelle::(!maclass);;
maclass := louis::(!maclass);;
maclass := lucien::(!maclass);;

let addEleveParNom uneClasse unEleve = 
    let rec addEleveParNomRec uneClasse unEleve = match uneClasse with
        |[] -> [unEleve]
        |hd::q -> if unEleve.nom<=hd.nom then unEleve::uneClasse
                else hd::(addEleveParNomRec q unEleve)
    in uneClasse := addEleveParNomRec (!uneClasse) unEleve;;

let trierParMoyenne uneClasse = 
    let rec addClasseParMoyenneRec uneClasse unEleve = match uneClasse with
        |[] -> [unEleve]
        |hd::q -> if unEleve.moygal<=hd.moygal then unEleve::uneClasse
                else hd::(addClasseParMoyenneRec q unEleve)
    in 
    let rec trierParMoyenneRec uneClasse = match uneClasse with
        |[] ->  []
        |hd :: q -> addClasseParMoyenneRec (trierParMoyenneRec q) hd
    in uneClasse := trierParMoyenneRec !uneClasse;;

let moyenneGeneral uneClasse =
    let rec moyenneGeneralrec uneClasse n = match uneClasse with
        |[] -> failwith "Liste vide"
        |[e] -> e.moygal, (n +. 1.) 
        |e::q -> let moy, n2 = moyenneGeneralrec q (n+.1.) in (e.moygal +. moy) , n2
    in let moy, n = moyenneGeneralrec !uneClasse 0. in moy /. n;; 

let moyenneMin uneClasse = match !uneClasse with
    |[] -> failwith "liste vide"
    |[e] -> e.moygal
    |hd::q -> let rec moyenneMinRec part min = match part with
            |[] -> min
            |e::q -> if e.moygal<min then moyenneMinRec q e.moygal
                    else  moyenneMinRec q min
            in moyenneMinRec q (hd.moygal);;
