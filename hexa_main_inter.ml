(* Modules *)
#use "modules/init.ml";;
#use "modules/utils.ml";;
#use "modules/lists.ml";;
#use "modules/maze.ml";;
#use "modules/hexa.ml";;
#use "modules/display.ml";;

(* Fonction pour obtenir aléatoirement une case de la grille hexaonale *)
let get_random_coord = fun r -> 
  let x = Random.int r in
  let y = (
    if x = 0 then
      0
    else
      Random.int (x * 6)
  ) in
  (x, y)
;;

(* Lancement du programme *)
let r = 5;;  (* Nombre de couronnes *)
let v_in = get_random_coord r;; (* Entrée *)
let v_out = get_random_coord r;; (* Sortie *)
let g = create_grid r;;
let m = generate_maze g;;
test_interactif (1920,1080) (2,600,350) m v_in v_out;;