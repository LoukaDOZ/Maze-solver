(* Modules *)
#use "modules/init.ml";;
#use "modules/utils.ml";;
#use "modules/lists.ml";;
#use "modules/maze.ml";;
#use "modules/rect.ml";;
#use "modules/display.ml";;

(* Lancement du programme *)
let w = 10;;  (* Largeur *)
let h = 10;;  (* Hauteur *)
let v_in = (Random.int w, Random.int h);; (* Entrée *)
let v_out = (Random.int w, Random.int h);;  (* Sortie *)
let g = create_grid w h;;
let m = generate_maze g;;
test_interactif (1920,1080) (4,0,0) m v_in v_out;;