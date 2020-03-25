open Graphics;;
open Random;;
open Printf;;

let left = 0.;;
let right = 300.;;
let down = 0.;;
let up = 500.;;
let w = int_of_float (left +. right);;
let h = int_of_float (down +. up);;

let ball = 5;; (* rayon de la balle *)
let paddle = 40;; (* largeur de la raquette *)
let thick = 4;; (* épaisseur de la raquette *)
let vx = ((Random.float 1.) -. 0.5)/.20.;;
let vy = ((Random.float 1. ) -. 0.5)/.20.;;

printf "vx = %f ; vy = %f \n" vx vy;; (*vérifier que le random fonctionne*)

(* Fonction qui dessine une balle de largeur ball
 * float x la position en x
 * float y la position en y
 *)
let draw_ball x y =
    set_color (rgb 0 0 0 );
    let x' = int_of_float x in
    let y' = int_of_float y in
    Graphics.fill_circle x' y' ball;;

(* Fonction qui dessine la raquette en bas de l'écran
 * int x la position x
 *)
let draw_paddle x =
   set_color (rgb 0 0 0 );
   Graphics.fill_rect (x) (0) (paddle) (thick);;

(* Fonction qui renvoie l'ordonnée int x de la souris pour la position de la raquette
 *)
let position_paddle () =
    let (x, y) = Graphics.mouse_pos () in x;;

(* Fonction qui renvoie la position float x au tps + 1
 * float x la position actuelle
 * float vx la vitesse en x
 *)
let new_position_x x vx =
    x +. vx;;

(* Fonction qui renvoie la position float y au tps + 1
 * float y la position actuelle
 * float vy la vitesse en y
 *)
let new_position_y y vy =
    y +. vy;;

let bouce_x x vx =
    if (x <= left || x >= right) then -.vx else vx;;



let rec algorithme x y vx vy =
    Graphics.clear_graph ();
    draw_ball x y;
    draw_paddle (position_paddle () ) ;
    Graphics.synchronize ();
    let vx' = bouce_x x vx in
    let vy' = if (x <= left || x >= right) then -.vx else vx in
    let x' = new_position_x x vx' in
    let y' = new_position_y y vy' in
    algorithme x' y' vx' vy';;



(* Ouvertude la fenetre *)

let dim = Printf.sprintf " %dx%d" w h ;;

open_graph dim; Graphics.auto_synchronize false;;(*openning windows*)
algorithme 200. 200. vx vy;;

