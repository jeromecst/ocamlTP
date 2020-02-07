open Printf
open Random

let rd = Random.self_init ();;
let borne = 1000;;
let nb = Random.int borne;;
printf "random number : %d \n" nb;;

let devine n =
    let rec devine etapes =
        let etapes' = etapes - 1 in
        match etapes with
        | 0 -> printf "vous avez perdu ! \n"
        | _ -> (
        printf "entrer un nombre entre 0 et %d : " borne;
        let u = read_int() in
        printf "\n";
        if n = u then (printf "Vous avez gagné !\n")
        else (
        if n > u then (printf "C'est plus ! il reste %d tentatives \n" etapes')
        else  (printf "C'est moins ! il reste %d tentatives \n" etapes')
        ;devine etapes'
        )
        )
    in
    devine 10;;


devine nb;;