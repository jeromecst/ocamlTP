(* Coquisart Jérôme 
  Groupe : Groupe 1 *)

open Printf
type couleur = Pique | Carreau | Coeur | Trefle
type valeur = Roi | Reine | Valet | Nombre of int
(* avec Nombre n tel que 1 ≤ n ≤ 10  *)
type carte = { couleur : couleur; valeur : valeur }

(* Donner un exemple de carte. *)

let c = {couleur = Pique ; valeur = Reine };;

(* Donner le type somme donnant le résultat d'une comparaison. *)

type comparaison = Joueur1 | Joueur2 | Tie;;

let meilleure_carte c1 c2 = 
        let val2 = c2.valeur in
        match c1.valeur with
        | Nombre(1) -> (
                        match val2 with
                        | Nombre(1) -> Tie
                        | _ -> Joueur1 )
        | Roi -> (
                        match val2 with
                        | Nombre(1) -> Joueur2
                        | Roi -> Tie
                        | _ -> Joueur1 )
        | Reine ->  (
                        match val2 with
                        | Nombre(1) -> Joueur2
                        | Roi -> Joueur2
                        | Reine -> Tie
                        | _ -> Joueur1 )
        | Valet -> (
                        match val2 with
                        | Nombre(1) -> Joueur2
                        | Roi -> Joueur2
                        | Reine -> Joueur2
                        | Valet -> Tie
                        | _ -> Joueur1 )
        | Nombre(i) ->
                        match val2 with
                        | Nombre(1) -> Joueur2
                        | Roi -> Joueur2
                        | Reine -> Joueur2
                        | Valet -> Joueur2
                        | Nombre(j) -> if i = j then Tie else (if i < j then Joueur2 else Joueur1 );;


let est_paire c1 c2 = 
        c1.valeur = c2.valeur ;;

let est_brelan c1 c2 c3 =
        c1.valeur = c2.valeur && c3.valeur = c1.valeur;;

let est_couleur c1 c2 c3 =
        c1.couleur = c2.couleur && c3.couleur = c1.couleur;;


let tirage_valeur () = 
        Random.self_init ();
        let rd = (Random.int 13) + 1 in
        match rd with
        | 13 -> Roi
        | 12 -> Reine
        | 11 -> Valet
        | i -> Nombre(i);;

let tirage_couleur () =
        Random.self_init ();
        let rd = (Random.int 4) + 1 in
        match rd with
        | 1 -> Pique
        | 2 -> Carreau
        | 3 -> Coeur
        | _ -> Trefle;;


let tirage_carte () = 
        { couleur = tirage_couleur () ; valeur = tirage_valeur () };;

let length carte_list =
        let rec length carte_list acc =
                match carte_list with
                | [] -> acc
                | hd::tl -> length tl (acc+1)
        in
        length carte_list 0
;;

let rec add_liste carte_list carte =
                match carte_list with
                | [] -> (carte::[])
                | hd::tl -> (hd::(add_liste tl carte))
;;

let affiche_carte carte =
        let printval = 
                match carte.valeur with
                | Roi -> printf "Roi "
                | Reine -> printf "Reine "
                | Valet -> printf "Valet "
                | Nombre(i) -> printf "%d " i
        in
        printval; printf "de ";
        match carte.couleur with
        | Pique -> printf "Pique "
        | Coeur -> printf "Coeur "
        | Trefle -> printf "Trefle "
        | Carreau -> printf "Carreau "
;;

let rec affiche_liste liste_carte = 
        match liste_carte with 
        | [] -> printf "\n\n"
        | hd::tl -> affiche_carte hd ; printf "- "; affiche_liste tl 
;;



let play carte_list_1 carte_list_2= 
        let rec play carte_list_1 carte_list_2 main =
                let message () = match main with 
                | true -> printf "vvvvvvvvvvv Joueur 1 à la main vvvvvvvvvvv\n"
                | false -> printf "vvvvvvvvvvv Joueur 2 à la main vvvvvvvvvvv\n" in
                message ();
                let winJ1 c1 c2 =
                        affiche_liste carte_list_1 ;
                        affiche_liste carte_list_2 ;
                        printf "^^^^^^^^^^^ Joueur 1 remporte la manche ^^^^^^^^^^^\n\n";
                        let carte_list_1 = add_liste (List.tl carte_list_1) c1 in
                        let carte_list_1 = add_liste carte_list_1 c2 in
                        let carte_list_2 = List.tl carte_list_2 in
                        play carte_list_1 carte_list_2 (not main)
                in

                let winJ2 c1 c2 = 
                        affiche_liste carte_list_1 ;
                        affiche_liste carte_list_2 ;
                        printf "^^^^^^^^^^^ Joueur 2 remporte la manche ^^^^^^^^^^^\n\n";
                        let carte_list_1 = List.tl carte_list_1 in
                        let carte_list_2 = add_liste (List.tl carte_list_2) c1 in
                        let carte_list_2 = add_liste carte_list_2 c2 in
                        play carte_list_1 carte_list_2 (not main)
                in      

                let tirage_carte carte_list = 
                        List.hd carte_list in

                if length carte_list_1 = 0 then printf "Joueur 2 gagne la partie\n" else (
                        if length carte_list_2 = 0 then printf "Joueur 1 gagner la partie\n" else  (
                                let c1 = tirage_carte carte_list_1 in
                                let c2 = tirage_carte carte_list_2 in
                                printf "Carte joueur 1 : " ; affiche_carte c1;
                                printf "Carte joueur 2 : " ; affiche_carte c2;
                                match meilleure_carte c1 c2 with
                                | Joueur1 -> 
                                                winJ1 c1 c2
                                | Joueur2 ->
                                                winJ2 c1 c2
                                | Tie ->
                                                if main then winJ1 c1 c2 else winJ2 c1 c2
                        )
                )
        in
        printf "...Début du jeu...\n";
        play carte_list_1 carte_list_2 true
;;

let rec carte_presente c1 carte_list =
        match carte_list with
        | [] -> false
        | hd::tl -> if hd = c1 then true else carte_presente c1 tl;;

let casse_liste carte_list =
        let len = length carte_list in
        let h_len = match len mod 2 with
                | 0 -> len / 2 
                | _ -> (len + 1) / 2 
        in
        let coupe_liste carte_list =
                let rec coupe_liste carte_list curr_len =
                        match carte_list with
                        | [] -> []
                        | hd::tl -> 
                                        if curr_len >= h_len then []
                                        else (hd::(coupe_liste tl (curr_len + 1)))
                        in
                        coupe_liste carte_list 0
        in
        let rec create_liste_2 carte_list_1 carte_list_2 cl1 curr_len =
                match carte_list_1 with
                | [] -> (coupe_liste cl1 , carte_list_2)
                | hd::tl -> 
                                if curr_len >= h_len then create_liste_2 tl (hd::carte_list_2) cl1 (curr_len + 1)
                                else create_liste_2 tl carte_list_2 cl1 (curr_len + 1)
        in
        create_liste_2 carte_list [] carte_list 0;;


let tirage_pile n =
        let len_fin = n in
        let rec remplir_liste carte_list curr_len = 
                if  (length carte_list ) < len_fin then (
                        let c = tirage_carte () in
                        if carte_presente c carte_list then remplir_liste carte_list curr_len
                        else remplir_liste (c::carte_list) (curr_len + 1)
                ) else carte_list
        in
        let c = tirage_carte () in
        let list1 = remplir_liste (c::[]) 0 in
        casse_liste list1 ;;
        


let play_bataille carte_list_1 carte_list_2= 
        let add_tas carte_list tas =
                let rec add_tas carte_list tas =
                        match carte_list with
                        | [] -> tas
                        | hd::tl -> hd::(add_tas tl tas)
                in
                add_tas carte_list tas
        in

        let rec play_bataille carte_list_1 carte_list_2 tas =
                let winJ1 c1 c2 tas =
                        printf "liste J1 :\n";
                        affiche_liste carte_list_1 ;
                        printf "liste J2 :\n";
                        affiche_liste carte_list_2 ;
                        printf "liste tas :\n";
                        affiche_liste tas ;
                        printf "^^^^^^^^^^^ Joueur 1 remporte la manche ^^^^^^^^^^^\n\n";
                        let carte_list_1 = add_liste (List.tl carte_list_1) c1 in
                        let carte_list_1 = add_liste carte_list_1 c2 in
                        let carte_list_2 = List.tl carte_list_2 in
                        let carte_list_1 = add_tas carte_list_1 tas in
                        play_bataille carte_list_1 carte_list_2 []
                in

                let winJ2 c1 c2 tas = 
                        printf "liste J1 :\n";
                        affiche_liste carte_list_1 ;
                        printf "liste J2 :\n";
                        affiche_liste carte_list_2 ;
                        printf "liste tas :\n";
                        affiche_liste tas ;
                        printf "^^^^^^^^^^^ Joueur 2 remporte la manche ^^^^^^^^^^^\n\n";
                        let carte_list_1 = List.tl carte_list_1 in
                        let carte_list_2 = add_liste (List.tl carte_list_2) c1 in
                        let carte_list_2 = add_liste carte_list_2 c2 in
                        let carte_list_2 = add_tas carte_list_2 tas in
                        play_bataille carte_list_1 carte_list_2 []
                in      

                let tirage_carte carte_list = 
                        List.hd carte_list in

                if length carte_list_1 = 0 then printf "Joueur 2 gagne la partie\n" else (
                        if length carte_list_2 = 0 then printf "Joueur 1 gagner la partie\n" else  (
                                let c1 = tirage_carte carte_list_1 in
                                let c2 = tirage_carte carte_list_2 in
                                match meilleure_carte c1 c2 with
                                | Joueur1 -> 
                                                printf "Carte joueur 1 : " ; affiche_carte c1; printf "\n\n";
                                                printf "Carte joueur 2 : " ; affiche_carte c2; printf "\n\n";
                                                winJ1 c1 c2 tas
                                | Joueur2 ->
                                                printf "Carte joueur 1 : " ; affiche_carte c1; printf "\n\n";
                                                printf "Carte joueur 2 : " ; affiche_carte c2; printf "\n\n";
                                                winJ2 c1 c2 tas
                                | Tie ->
                                                printf "Carte joueur 1 : " ; affiche_carte c1; printf "\n\n";
                                                printf "Carte joueur 2 : " ; affiche_carte c2; printf "\n\n";
                                                printf "\n\n\ ----------------------------BATAILLE ----------------------------\n\n";
                                                let tas = c1::c2::tas in
                                                let carte_list_1 = List.tl carte_list_1 in
                                                let carte_list_2 = List.tl carte_list_2 in
                                                let c1 = tirage_carte carte_list_1 in
                                                let c2 = tirage_carte carte_list_2 in
                                                let tas = c1::c2::tas in
                                                play_bataille (List.tl carte_list_1) (List.tl carte_list_2) tas 
                        )
                )
        in
        play_bataille carte_list_1 carte_list_2 []
;;

let tests () =
        let rec while_affiche_carte n =
                match n with 
                | 0 -> ()
                | _ ->  affiche_carte (tirage_carte () ) ; while_affiche_carte (n-1)
        in
        while_affiche_carte 10;

        let c0  = {couleur = Pique ; valeur = Nombre(1) } in
        let c1  = {couleur = Coeur ; valeur = Nombre(1) } in
        let c2  = {couleur = Trefle ; valeur = Nombre(2) } in
        let c3  = {couleur = Carreau ; valeur = Nombre(3) } in
        let c4  = {couleur = Carreau ; valeur = Nombre(4) } in
        let c5  = {couleur = Carreau ; valeur = Nombre(5) } in
        let c6  = {couleur = Carreau ; valeur = Nombre(6) } in
        let c7  = {couleur = Carreau ; valeur = Nombre(7) } in
        let c8  = {couleur = Carreau ; valeur = Nombre(8) } in
        let c9  = {couleur = Carreau ; valeur = Nombre(9) } in
        let c10 = {couleur = Coeur ; valeur = Nombre(10) } in
        let c14 = {couleur = Coeur ; valeur = Reine } in
        let c15 = {couleur = Coeur ; valeur = Roi } in
        let c16 = {couleur = Coeur ; valeur = Valet } in
        let c17 = {couleur = Coeur ; valeur = Valet } in
        let c18 = {couleur = Coeur ; valeur = Valet } in
        let c19 = {couleur = Trefle ; valeur = Roi } in
        let c20 = {couleur = Trefle ; valeur = Roi } in
        let c21 = {couleur = Trefle ; valeur = Roi } in

        printf "%b " ((est_couleur c10 c14 c15 ) = true) ;
        printf "%b " ((est_couleur c5 c14 c15 ) = false) ;

        printf "%b " ((est_paire c16  c17 ) = true) ;
        printf "%b " ((est_paire c20  c21 ) = true) ;
        printf "%b " ((est_paire c0  c1 ) = true) ;
        printf "%b " ((est_paire c10  c20 ) = false) ;

        printf "%b " ((est_brelan c19  c20 c21) = true) ;
        printf "%b " ((est_brelan c16  c17 c18) = true) ;
        
        printf "%b " ((meilleure_carte c0  c0 ) = Tie) ;
        printf "%b " ((meilleure_carte c2  c2 ) = Tie) ;
        printf "%b " ((meilleure_carte c4  c4 ) = Tie) ;
        printf "%b " ((meilleure_carte c1  c1 ) = Tie) ;
        printf "%b " ((meilleure_carte c0  c1 ) = Tie) ;
        printf "%b " ((meilleure_carte c1  c2 ) = Joueur1) ;
        printf "%b " ((meilleure_carte c2  c3 ) = Joueur2) ;
        printf "%b " ((meilleure_carte c3  c4 ) = Joueur2) ;
        printf "%b " ((meilleure_carte c4  c5 ) = Joueur2) ;
        printf "%b " ((meilleure_carte c5  c6 ) = Joueur2) ;
        printf "%b " ((meilleure_carte c6  c7 ) = Joueur2) ;
        printf "%b " ((meilleure_carte c7  c8 ) = Joueur2) ;
        printf "%b " ((meilleure_carte c8  c9 ) = Joueur2) ;
        printf "%b " ((meilleure_carte c9  c10) = Joueur2) ;


        let (l1, l2) = tirage_pile 7 in
        printf "test tirage pile :\n";
        printf "l1 :\n";
        affiche_liste l1;
        printf "\n l2:\n";
        affiche_liste l2;

        play l1 l2;

       
        printf "\n"
;;

(*tests () ;;*)


let (l1, l2) = tirage_pile 52 in

printf "\n\n\ ----------------------------BEGIN PLAY BATAILLE ----------------------------\n\n";
play_bataille l1 l2 ;

