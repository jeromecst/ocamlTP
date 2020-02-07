open Printf

let div_between a b n =
    let rec div i =
        if i <= b then
        ( if n mod i = 0 then true else div (i + 1) )
        else false
        in
    if a = 0 then div 1 else div a
;;

let isPrime n =
    match n with
    | 0 -> false
    | 1 -> false
    | _ -> not (div_between 2 (int_of_float ( sqrt (float_of_int n) ) + 1) n);;

if isPrime 17 then printf "true\n" else printf "false\n";;
if isPrime 10 then printf "true\n" else printf "false\n";;
if isPrime 97 then printf "true\n" else printf "false\n";;
