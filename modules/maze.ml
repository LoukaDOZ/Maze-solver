(* Fonctions communes de travail sur les labyrinthes : création, résolution *)

(* Ajoute un sommet v au graphe g (sans successeur) et renvoie le nouveau graphe
Si v est déjà dans g, g est directement renvoyé *)
let add_vertex = fun v -> fun g ->
  if (CellMap.mem v g) = false then
      CellMap.add v CellSet.empty g
  else
      g
;;

(* Ajoute les arretes u vers v et v vers u au graphe g s'il n'existent pas déjà 
et renvoie le nouveau graphe *)
let add_edges = fun v -> fun u -> fun g ->
      let g = (
          if ( CellSet.mem u (CellMap.find v g) ) = false then
              CellMap.add v ( CellSet.add u (CellMap.find v g) ) g
          else
              g
      ) in 
      if ( CellSet.mem v (CellMap.find u g) ) = false then
          CellMap.add u ( CellSet.add v (CellMap.find u g) ) g
      else
          g
;;

(* Retourne un nouveau labyrinthe avec au minimum les mêmes murs que m *)
let rec generate_maze_aux = fun g -> fun m -> fun v -> fun l ->
  match l with
      | [] -> m
      | v'::t -> 
          if (CellMap.mem v' m) then
              generate_maze_aux g m v t
          else
              let m' = (let m2' = add_vertex v' m in add_edges v' v m2') in
              let l' = (let l2' = CellSet.elements (CellMap.find v' g) in suffle l2') in
              let m'' = generate_maze_aux g m' v' l' in
              generate_maze_aux g m'' v t
;;

(* Retourne un labyrinthe sur la grille g *)
let generate_maze = fun g ->
  match (CellMap.choose g) with
      | (k, s) -> 
          generate_maze_aux g 
            (CellMap.add k CellSet.empty CellMap.empty) k (CellSet.elements s)
;;

(* Fonction auxiliaire de solve_maze *)
let rec solve_maze_aux = fun m -> fun l -> fun p -> fun v -> fun s ->
  (* Ajout de la case actuelle à la liste des sommets déjà parcourus *)
  let p = p @ [v] in
  let set = CellMap.find v m in
  (* Si la sortie est voisine accessible de la case actuelle *)
  if (CellSet.mem s set) then
        (* Terminer : labyrinthe résolu *)
      l @ (v::s::[])
  else
      (* Sinon, pour chaque voisins accessibles de la case actuelle *)
      let l' = List.fold_left
          (fun acc v' -> 
              (* Si le voisin n'a pas déjà été parcouru et
              on vérifie aussi qu'on a pas déjà trouvé la sortie pour éviter de relancer
              la récusivité sur les voisins restants, aors qu'on à déjà un chemin valide *)
              if (List.mem v' p) = false && (List.mem s acc) = false then
                  (* Continuer sur ce chemin *)
                  let l'' = solve_maze_aux m l p v' s in
                  (* Si la sortie a été trouvée *)
                  if (List.mem s l'') then
                      (* Terminer : labyrinthe résolu *)
                      acc @ l''
                  else
                      (* Sinon, essayer le voisin suivant *)
                      acc
              else
                  (* Sinon, essayer le voisin suivant *)
                  acc
          )
          [] (CellSet.elements set) in
      l @ [v] @ l'
;;

(* Résoud le labyrinthe m et retourne une liste de toutes les cases du chemin trouvé 
* à parcourir en partant de l'entrée jusqu'à la sortie
* Parcours le labyrinthe en profondeur : explore toutes les possibilités depuis l'entrée
* jusqu'à trouver la sortie. Retourne le premier chemin trouvé
* Trouve toujours une solution mais pas forcément la plus courte/simple *)
let solve_maze = fun m -> fun e -> fun s ->
  solve_maze_aux m [] [] e s
;;