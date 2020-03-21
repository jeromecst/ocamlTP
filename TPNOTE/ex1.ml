(* Coquisart Jérôme 
  Groupe : Groupe 1 *)

(*
Question 1:
        La fonction renvoie 2 car la ligne "y r> y" crée une nouvelle variable y qui prend comme valeur "x=2" et renvoie 2, si on voulait comparer x à y il faut passer par un "if"

Question 2:
        La différence est que si x n'est pas 1 ou 2, alors dans la fonction f1 on renvoie 7, dans la fonction f2 on renvoie 7 mais y prend la valeur de x (même si y n'est utilisé ici)

Question 3:

*)

let f3 x =
        match x with
        | 1 -> 6;;
(*

Question 4:
        La fonction r1 ne l'est pas, on ajoute 10 à chaque appel, cette valeur n'est pas dans un accumulateur

        La fonction r2 ne l'est pas, car le resultat de r2 doit être comparé avec la valeur de a (par un OU)

        La fonction aux de r3 l'est, il y a un accumulateur "acc" et aucune opération n'est effectué avec le résultat de aux

        La fonction r4 n'est pas récursive.


Question 5:

          "let y = x + 3" devrait être --->>  "let y = x + 3 in"
          - justification : les variables déclarée dans les fonction sont locales, d'où 'in'
          
         "if x < y then  Printf.printf "%d" x; x else y" devrait être --->> "if x < y then  (Printf.printf "%d" x; x) else y"
         - justification : l'appel if doit renvoyer une seule instruction, ici il faut mettre des parenthèses car la seule instruction est décomposée en deux instructions

         "return (v + 10)" devrait être --->> "(v + 10)"
         - justification : le "=" après "g x y" correspond au return, il n'y a donc pas besoin de préciser return.

 *)
