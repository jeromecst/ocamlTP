open Printf;;

type name = {nom : string ; prenom : string};;
type anniv = {jour : int ; mois : int ; annee : int};;
type person = {name : string ; anniv : anniv};;

let print_name name =
        let (nom, prenom) = name in
        printf "nom : %s, prenom : %s" nom prenom;;

let print_anniv anniv =
        let (jour, mois, annee) = anniv in
        printf "%d %d %d" jour mois annee;;

let print_person person =
        let (name, anniv) = person in
        print_anniv anniv;
        print_anniv name;;

type rps = Pierre | Papier | Ciseaux;;

let qui_gagne rps =
        match rps with 
        | (Pierre, Pierre) -> printf "TIE\n"
        | (Papier, Papier) -> printf "TIE\n"
        | (Ciseaux, Ciseaux) -> printf "TIE\n"
        | (Pierre, Ciseaux) -> printf "Gagant Pierre\n"
        | (Ciseaux, Pierre) -> printf "Gagant Pierre\n"
        | (Papier, Pierre) -> printf "Gagant Papier\n"
        | (Pierre, Papier) -> printf "Gagant Papier\n"
        | (Ciseaux, Papier) -> printf "Gagant Ciseaux\n"
        | (Papier, Ciseaux) -> printf "Gagant Ciseaux\n"
;;

qui_gagne (Pierre, Pierre);;
qui_gagne (Pierre, Ciseaux);;
qui_gagne (Ciseaux, Pierre);;
qui_gagne (Papier, Pierre);;

type t = Fruit | Legume;;

let l = [ ("pomme", Fruit); ("orange", Fruit); ("banane", Fruit); ("citron", Fruit); ("poire", Fruit); ("pomme de terre", Legume); ("tomate", Legume); ("courgette", Legume) ]

let print_fruit l = 
        let rec parcour_liste l =
                match l with
                | [] -> printf "\n"
                (* head and tail *)
                | hd::tl ->
                        let ( nom_fruit, type_fruit ) = hd in
                        if type_fruit = Fruit then (printf "%s " nom_fruit);
                        parcour_liste tl (* tail *)
        in
        parcour_liste l
;;

print_fruit l;;

(*
 * Fonction qui renvoie une liste
 * l1 de fruit et l2 de légumes
 * à partir d'une liste contenant
 * des fuits et des légumes
 *)
let fruit_legume l = 
        let rec parcour_liste l l1 l2 =
                match l with
                | [] -> (l1, l2)
                | head::tail ->
                        let ( nom, typelf) = head in
                        if typelf = Fruit then
                                parcour_liste tail (head::l1) l2
                        else 
                                parcour_liste tail l1 (head::l2)
        in
        parcour_liste l [] []
;;

let rec print_list l =
        match l with
        | [] -> printf "\n"
        | hd::tl -> printf "%s " (fst hd) ; print_list tl;;

let (l1, l2) = fruit_legume l;;
print_list l1;;
print_list l2;;

type t1 = F of string | L of string;;

let to_t1 hd = 
        let (name,lftype) = hd in
        match lftype with 
        | Fruit -> F(name)
        | Legume -> L(name);;

to_t1 ("pomme", Fruit);;

let convert_list l =
        let rec convert_list l l' = 
                match l with 
                | [] -> l'
                | hd::tl ->
                                let typetone = to_t1 hd in
                                convert_list tl (typetone::l')
        in
        convert_list l []
;;

let l' = convert_list l ;;

let exist fruitleg l =
        let rec parcour_liste l =
                match l with
                | [] -> false
                | hd::tl -> if hd = fruitleg then true else (parcour_liste tl)
        in
        parcour_liste l;;


let valleur = exist (F "orange") l';;
let valleur = exist (F "tomate") l';;
printf "%b\n" valleur;;

let count l = 
        let rec parcour_liste l (f,leg) = 
                match l with 
                | [] -> (f,leg)
                | hd::tl -> match hd with
                                | F(n) -> parcour_liste tl (f+1, leg)
                                | L(n) -> parcour_liste tl (f, leg+1)
        in
        parcour_liste l (0,0);;

let (l, g) = count l';;
printf "%d fruits et %d legumes\n" l g ;;


