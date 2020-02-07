open Printf

let print_decomp n base =
    let rec func i str =
        match i with
        | 0 -> str
        | _ ->
            let reste = i mod base in
            let str' = (string_of_int reste) ^ str in
            func ((i - reste) / base) str'
    in
    let str2 = func n "" in
    printf "%s\n" str2
    ;;

(* base 2*)
print_decomp 23 2;;
print_decomp 103 2;;
print_decomp 1034029234 2;;

(* base 8*)
print_decomp 23 8;;
print_decomp 103 8;;
print_decomp 1034029234 8;;

(* base 5*)
print_decomp 23 5;;
print_decomp 103 5;;
print_decomp 1034029234 5;;
