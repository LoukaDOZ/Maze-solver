type status = Input | Output | Inside ;;


let color_of_status = fun st -> match st with
  | Input  -> Graphics.green
  | Output -> Graphics.red
  | Inside -> Graphics.rgb 192 192 192 (* light gray *)
;;


let do_the_thing = fun f -> fun u -> fun v ->
  match List.map f (get_wall u v) with
  | [(x1,y1); (x2,y2)] ->
     let _ = Graphics.moveto x1 y1 in
     Graphics.lineto x2 y2
  | _ -> ()
;;


let draw_vertex = fun f -> fun st -> fun v -> fun succ ->
  let contour  = List.map f (get_contour v) in
  let contour' = Array.of_list contour in
  let _ = Graphics.set_color (color_of_status st) in
  let _ = Graphics.fill_poly contour' in
  let _ = Graphics.set_color Graphics.black in
  let _ = Graphics.draw_poly contour' in
  let _ = Graphics.set_color (color_of_status st) in
  let _ = CellSet.iter (fun u -> do_the_thing f u v) succ in
  Graphics.set_color Graphics.black
;;


let get_status = fun v_in -> fun v_out -> fun v ->
  if v = v_in then       Input
  else if v = v_out then Output
  else                   Inside
;;


let init = fun w -> fun h ->
  let _ = Graphics.open_graph "" in
  let _ = Graphics.resize_window w h in
  Graphics.clear_graph ()
;;

let wait_and_close = fun () ->
  let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
  Graphics.close_graph ()
;;


let draw_maze = fun f -> fun g -> fun v_in -> fun v_out ->
  CellMap.iter (fun v succ -> draw_vertex f (get_status v_in v_out v) v succ) g
;;


let rec draw_path_aux = fun f -> fun g -> fun v -> fun succ -> fun path ->
  match path with
  | [] -> ()
  | v'::vs ->
    if CellSet.mem v' succ then
      try
        let succ' = CellMap.find v' g in
        let x, y  = f (get_center_coord v) in
        let x',y' = f (get_center_coord v') in
        let _ = Graphics.moveto x y in
        let _ = Graphics.lineto x' y' in
        draw_path_aux f g v' succ' vs
      with _ ->
        let (i',j') = v' in Printf.printf "(%d,%d) is outside of the maze\n" i' j'
    else
      let (i,j) = v in
      let (i',j') = v' in
      Printf.printf "No path from (%d,%d) to (%d,%d)\n" i j i' j'
;;

let draw_path = fun f -> fun g -> fun path -> match path with
  | [] -> ()
  | v::vs ->
    try
      let succ = CellMap.find v g in
      let _ = Graphics.set_color Graphics.blue in
      draw_path_aux f g v succ vs
    with _ ->
      let (i,j) = v in Printf.printf "(%d,%d) is outside of the maze\n" i j
;;

let test = fun (w,h) -> fun (z,sw,sh) -> fun g -> fun v_in -> fun v_out -> fun path ->
  let _ = init w h in
  let f = fun (x,y) -> (z*x+sw, z*y+sh) in
  let _ = draw_maze f g v_in v_out in
  let _ = draw_path f g path in
  wait_and_close ()
;;

(* Fonction auxiliaire de test interactif *)
let rec test_interactif_aux = fun f -> fun g -> fun v_in -> fun v_out -> fun path ->
  (* Dessiner le graphe *)
  let _ = Graphics.clear_graph () in
  let _ = draw_maze f g v_in v_out in
  let _ = draw_path f g path in
  
  (* Récupérer la position actuelle *)
  let pos = List.nth path ((List.length path) - 1) in
  (* Connaitre si le joueur a trouvé la sortie *)
  let finished = (match v_out with
    | (x, y) -> 
      match pos with
        | (px, py) -> 
          (* Si les positions de la case actuelle et celles de la sortie sont
          les même alors le joueur a fini *)
          if x = px && y = py then
            true
          else
            false
  ) in

  (* Terminer si le joueur a trouvé la sortie (attendre l'appui sur n'importe quelle touche) *)
  if finished then
    wait_and_close ()
  else
    (* Fonction qui permet de savoir si le joueur a rebroussé chemin *)
    let do_go_back = fun (x, y) -> fun l ->
      (* Il faut que le joueur est déjà parcourus une certaine distance *)
      if (List.length l) < 2 then
        false
      else
        (* Si la position actelle est la même que la précédente, le joueur à fait demi-tour *)
        let px, py = List.nth path ((List.length path) - 2) in
        (x = px && y = py)
    in

    (* Fonction qui retire le dernier élément d'une liste *)
    let rec remove_last = fun l ->
      match l with
        | [] -> []
        | [h] -> []
        | h::t -> [h] @ (remove_last t)

    in

    (* Attendre que l'utilisateur presse une touche *)
    let ev = Graphics.wait_next_event [Graphics.Key_pressed] in
    (* Récupérer la touche sous forme d'entier *)
    let key = int_of_char ev.Graphics.key in
    match key with
      | 27 -> ()  (* Quitter si Echap *)
      | _ ->  (* Autres touches *)
        (* Position suivante *)
        let next = key_pressed key pos in

        (* Si la prochaine position est une voisin accessible de l'actuelle *)
        if ( CellSet.mem next (CellMap.find pos g) ) then
          (* Si l'utilisateur à fait demi-tour *)
          if (do_go_back next path) then
            (* Revenir en arrière *)
            test_interactif_aux f g v_in v_out (remove_last path)
          else
            (* Sinon, avancer *)
            test_interactif_aux f g v_in v_out (path @ [next])
        else
          (* Sinon, recommencer *)
          test_interactif_aux f g v_in v_out path
;;

(* Permet de lancer un test interactif sur le labyrinthe g *)
let test_interactif = fun (w,h) -> fun (z,sw,sh) -> fun g -> fun v_in -> fun v_out ->
  (* Initialisations *)
  let _ = init w h in
  let f = fun (x,y) -> (z*x+sw, z*y+sh) in
  (* Lancement du programme interactif *)
  test_interactif_aux f g v_in v_out [v_in]
;;