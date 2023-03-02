(* Fonctions propres à la grille rectangulaire *)

(* Retourne true si la case c appartient bien à la grille de taille m * n, false sinon *)
let is_valid = fun m -> fun n -> fun c -> 
    match c with
        | (x, y) -> (x >= 0 && x < m && y >= 0 && y < n)
;;

(* Retourne la liste des voisins de la case c sur la grille m * n *)
let get_neighbours = fun m -> fun n -> fun c -> 
    match c with
        | (x, y) -> 
            (* Liste des voisins (Sud, Est, Nord, Ouest) *)
            let neighbours = (x, y - 1)::(x + 1, y)::(x, y + 1)::(x - 1, y)::[] in
            (* Vérifier la validité de chaque voisin, ne conserver que les valides *)
            List.fold_left 
                (fun acc ngb -> 
                    if (is_valid m n ngb) then 
                        acc @ [ngb]
                    else
                        acc
                ) 
                [] neighbours
;;

(* Retourne une grille de taille m * n *)
let create_grid = fun m -> fun n ->
    (* Ajout des cases *)
    let lc = range2 m n in
    let acc = List.fold_left 
            (fun acc c -> add_vertex c acc) 
            CellMap.empty lc
    in 
    
    (* Fonction qui lie c à ses voisins valides *)
    let add_neighbours_edges = fun c -> fun g ->
        List.fold_left
            (fun acc i -> add_edges c i acc)
            g (get_neighbours m n c)
    in 
    
    (* Fonction qui lie chaque case à ses voisins valides *)
    let rec set_edges = fun lc -> fun g ->
        match lc with
            | [] -> g
            | h::t -> set_edges t (add_neighbours_edges h g)
    in 
    
    (* Ajout des voisins *)
    set_edges lc acc
;;

(* Retourne un tuple (x,y) des cordonnées du centre de la case c *)
let get_center_coord = fun c ->
    match c with
        | (x, y) -> (x * 2 + 1, y * 2 + 1)
;;

(* Retourne une liste des cordonnées des 4 coins de la case c *)
let get_contour = fun c ->
    match (get_center_coord c) with
        | (x, y) -> (x - 1, y - 1)::(x - 1, y + 1)::(x + 1, y + 1)::(x + 1, y - 1)::[]
;;

(* Retourne une liste des cordonnées des coins communs aux cases c1 et c2 *)
let get_wall = fun c1 -> fun c2 ->
    (* Obtenir les contours des 2 cases *)
    let contour1 = get_contour c1 in
    let contour2 = get_contour c2 in

    (* Fonction qui retourne les positions des 2 points qui forment une face
    Les faces sont vont de 0 à 3 *)
    let get_face_coord = fun contour -> fun face ->
        (List.nth contour face)::(List.nth contour ((face + 1) mod 4))::[]
    in 
    
    (* Fonction qui compare 2 points (x, y) *)
    let compare_points = fun c1 -> fun c2 ->
        match c1 with
            | (x1, y1) -> match c2 with
                | (x2, y2) -> (x1 = x2 && y1 = y2)
    in 
    
    (* Fonction qui compare 2 faces *)
    let compare_faces = fun f1 -> fun f2 ->
        match f1 with
            | [] | _::[] -> false
            | p1f1::p2f1::_ -> match f2 with
                | [] | _::[] -> false
                | p1f2::p2f2::_ ->
                    ((compare_points p1f1 p2f2) && (compare_points p1f2 p2f1))
    in

    (* Pour chaque faces de 0 à 3 de la case 1, la comparer avec sa face oposée de la case 2
    Si ce sont les mêmes, c'est que les faces sont "collées" *)
    List.fold_left
        (fun acc f -> 
            let f1 = get_face_coord contour1 f in
            if ( compare_faces f1 (get_face_coord contour2 ((f + 2) mod 4)) ) then
                f1
            else
                acc
        )
    [] (range 0 3)
;;

(* Retourne le voisin correspondant à la touche appuyée, qu'il soit valide ou non
Retourne (1, -1) pour toutes touches autres que z, q, s et d *)
let key_pressed = fun k -> fun (x, y) ->
    match k with
        | 122 -> (x, y + 1) (* z Nord *)
        | 113 -> (x - 1, y) (* q Ouest *)
        | 115 -> (x, y - 1) (* s Sud *)
        | 100 -> (x + 1, y) (* d Est *)
        | _ -> ((-1), (-1)) (* Autre *)
;;