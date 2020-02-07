open Printf

let mult a b =
    let rec mult acc i =
    if i = 0 then acc
    else mult (acc + b) (i - 1)
    in
    mult 0 a;;

let mult_match x y =
        let rec mult x acc =
        match x with
        | 0 -> 0
        | 1 -> acc
        | _ -> let acc' = acc + y in mult acc' (x - 1)
        in
        mult x 0;;

let result = mult 3 6 ;;
let result_rec = mult 6 6 ;;

printf "%d \n" result;;
printf "%d \n" result_rec;;

let exp_trivial e n =
    let rec exp n =
    match n with
    | 0 -> 1
    | _ -> e * exp (n - 1)
    in
    exp n;;

let exp e n =
    let rec exp i acc =
    match i with
    | 0 -> acc
    | _ -> exp (i-1) (acc * e)
    in
    exp n 1;;


let resultrivial = exp_trivial 4 4;;
printf "%d \n" resultrivial;;
let result = exp 4 4;;
printf "%d \n" result;;



