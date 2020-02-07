open Printf

let affiche_string n =
    printf "%s \n" n
;;

let affiche_int n =
    printf "%d \n" n
;;

let affiche_float n =
    printf "%f \n" n
;;

let distance_euclidienne x1 y1 x2 y2 =
    let diff1 = (x1 -. x2) *. (x1 -. x2) in
    let diff2 = (y1 -. y2) *. (y1 -. y2) in
    let somme = diff1 +. diff2 in
    sqrt somme;;

let f1 n =
    if n mod 2 = 0 then affiche_string "PAIR" else affiche_string "IMPAIR";;

let f2 n =
    if n mod 2 = 0 then affiche_string "PAIR" else affiche_string "IMPAIR";;

let f3 n =
    let str = if n mod 2 = 0 then "PAIR" else "IMPAIR" in
    affiche_string str;;

let sum n =
    let rec sum i acc =
        let acc' = acc + i in
        let i' = i - 1 in
        if i > 0 then sum i' acc' else affiche_int acc'
    in
    sum n 0
;;

let sum2 n =
    let rec sum2 i acc =
        let acc' = acc + i in
        let i' = i - 1 in
        match i with
        | 0 -> affiche_int acc'
        | _ -> sum2 i' acc'
    in
    sum2 n 0
;;

let even_odd x = if x mod 2 = 0 then "Pair" else "Impair"

let borne_sup x =
    match even_odd x with
    | "Pair" -> affiche_int(x / 2)
    | "Impair" ->  affiche_int((x + 1) / 2)
    ;;


affiche_string "Hello World!";;
let dst = distance_euclidienne 43. 32. 32. 54.;;
affiche_float dst;;
f1 2;;
f2 3;;
f3 4;;
sum 10;;
sum2 10;;
borne_sup 3;;
borne_sup 5;;
borne_sup 14;;
