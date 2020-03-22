(* Coquisart Jérôme 
  Groupe : Groupe 1 *)

(*
 * COMMENTAIRE = si certaines fonctions semblent un peu complexes, c'est parce-que j'ai fais en sorte 
 * que toutes les fonctions soient compatibles si on prend des listes de taille supérieure à 4, dans l'hypothèse
 * que toutes les proriétés sur des listes de taille 4 fonctionnent sur des listes de taille supérieure
 *)
let n = 4;;

open Printf

type coin = Head | Tails
type board = coin list
type plays = Flip of int

let print_board board =
        let rec print_board board =
                match board with
                | [] -> printf "\n"
                | head::tail -> 
                                match head with
                                | Head -> printf "Head " ; print_board tail
                                | Tails -> printf "Tails "; print_board tail
        in
        print_board board;;


(* Définir une fonction flip de type coin -> coin qui retourne une pièce. *)

let flip coin =
        match coin with
        | Head -> Tails
        | Tails -> Head
;;


let play flp board =
        let rec flip_i i board =
                match board with
                | [] -> board
                | head::tail -> if i !=0 then head::(flip_i (i-1) tail ) else (flip head)::tail
        in

        match flp with
        | Flip i -> 
                        let i' = (i+1) mod n in 
                        let b' = flip_i i' board in 
                        flip_i i b';;

(* Définir une fonction play_multiple de type plays list -> board -> board qui joue les coups dans l'ordre. *)

let play_multiple play_list board =
        let rec play_multiple play_list board =
                match play_list with
                | [] -> board
                | head::tail -> let b' = play head board in play_multiple tail b'
        in
        play_multiple play_list board;;



(* Définir une fonction is_winning de type board -> bool qui indique si le plateau est gagnant. *)

let is_winning board =
        let rec is_winning board =
                match board with
                | [] -> true
                | head::tail -> 
                                match head with
                                | Head -> is_winning tail
                                | Tails -> false;
        in
        if board = [] then false else is_winning board;;


let b = [Head; Tails; Head; Tails] ;;
let pl = [Flip(0);Flip(3)];;
print_board (play_multiple pl b);;
printf "%b\n" (is_winning (play_multiple pl b));;

(* Tous les plateaux n'ont pas de solution. On pourra admettre qu'un plateau a une solution si et seulement si le nombre de pile des pièces d'indice 0 et 2 a la même parité que le nombre de pile des pièces d'indice 1 et 3. *)

let has_sol board =
        let rec has_sol board npair nimpair n =
                match board with 
                | [] -> npair = nimpair 
                | head::tail -> 
                                match n with
                                | 0 -> has_sol tail ((npair+1)mod 2) nimpair 1
                                | _ -> has_sol tail npair ((nimpair+1)mod 2) 0
        in
        has_sol board 0 0 0 ;;

(*
printf "%b\n" (has_sol [Head ; Head ; Head ; Head]);;
printf "%b\n" (has_sol [Tails ; Head ; Tails ; Head]);;
printf "%b\n" (has_sol [Tails ; Tails ; Head ; Tails]);;
*)

let find_sol board =
        (*
         * Fonction qui renvoie la position du début de la pair
         * -1 si il n'y a pas de pair dans le board
         *)
        let has_pair board =
                let rec has_pair board head_tail pos =
                        match board with
                        | [] -> (-1, Head)
                        | head::tail -> if head = head_tail then (pos, head) else has_pair tail head (pos+1)
                in
                match board with
                | [] -> (-1, Head)
                | head::[] -> (-1, Head)
                | head::tail -> has_pair tail head 0
        in
        let rec add_list play_list play =
                match play_list with
                | [] -> play::[]
                | head::tail -> head::(add_list tail play)
        in
        let rec find_sol board play_list =
                if is_winning board then play_list else 
                match has_pair board with
                | (-1,_) -> 
                                let flp = Flip 1 in
                                let b' = play flp board in
                                find_sol b' (add_list play_list flp)
                | (pos, head_tail) -> 
                                match head_tail with
                                | Head -> 
                                                let flp = Flip ((pos+2) mod n) in
                                                let b' = play flp board in
                                                find_sol b' (add_list play_list flp)
                                | Tails -> 
                                                let flp = Flip pos in
                                                let b' = play flp board in
                                                find_sol b' (add_list play_list flp)
        in
        find_sol board [] ;;

let test_find_sol board =
        let sol = find_sol board in
        printf "%b : " (is_winning (play_multiple sol board));
        print_board board ;;

printf "begin tests find_sol\n";;
test_find_sol [Head; Head; Head; Head];;
test_find_sol [Head; Tails; Head; Tails];;
test_find_sol [Tails; Head; Tails; Head];;
test_find_sol [Head; Head; Tails; Tails];;
test_find_sol [Head; Tails; Tails; Head];;
test_find_sol [Tails; Tails; Head; Head];;
test_find_sol [Tails; Head; Head; Tails];;
test_find_sol [Tails; Tails; Tails; Tails];;

