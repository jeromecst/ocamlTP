(* Coquisart Jérôme 
  Groupe : Groupe 1 *)

open Printf

(* Question 1 *)

let gcd a b =
        let rec gcd a b =
                if a = b then a else (if a > b then gcd (a-b) b  else gcd b a)
        in
        gcd a b;;


(* Question 2 *)

printf "Le plus grand diviseur commun entre 24 et 64 est %d\n" (gcd 24 64)

(*
Justification :
       64 > 24 donc gcd(64, 24) (else)
       64 > 24 donc gcd(64-24, 24) = gdc(40, 24)
       40 > 24 donc gcd(40-24, 24) = gcd(16, 24)
       16 < 24 donc gcd(24, 16) (else)
       24 > 16 donc gcd(24-16, 16) = gcd(8, 16)
       8 < 16 donc gcd(16, 8) (else)
       16 > 8 donc gcd(16-8,8)=gcd(8,8)
       8=8 donc on renvoie 8
 *)

(* Question 3 *)

let prime a b =
        (gcd a b) = 1 ;;

let print_prime a b = 
        if prime a b then printf "%d et %d sont premiers entre eux\n" a b else printf "%d et %d ne sont pas premiers entre eux\n" a b ;;

print_prime 10 40;;
print_prime 5 7;;
print_prime 9 3;;
print_prime 5064 306;;
print_prime 1 69;;

(* BONUS : Question 4 *)

let phi n=
        let rec phi acc i =
                match i with
                | 0 -> acc
                | _ -> if prime i n then phi (acc+1) (i-1) else phi (acc) (i-1)
        in
        phi 0 n
;;

let n = 10;;
printf "Il y a %d nombres premiers avec %d entre 1 et %d\n" (phi n) n n ;;

let n = 1000;;
printf "Il y a %d nombres premiers avec %d entre 1 et %d\n" (phi n) n n ;;

let n = 53452;;
printf "Il y a %d nombres premiers avec %d entre 1 et %d\n" (phi n) n n ;;
