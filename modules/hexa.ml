(* Fonctions propres à la grille hexagonale *)

(* Retourne true si la case c appartient bien à la grille de rayon r, false sinon *)
let is_valid = fun r -> fun c ->
    match c with
        | (x, y) -> (x >= 0 && x < r)
;;

(* Retourne la liste des voisins de la case c sur la grille de rayon r *)
let get_neighbours = fun r -> fun c -> 
    match c with
        | (x, y) -> 
            let neighbours = (
                (* (0, 0) est un schéma unique *)
                if x = 0 && y = 0 then 
                    (1, 0)::(1, 1)::(1, 2)::(1, 3)::(1, 4)::(1, 5)::[]
                else
                    (* Si c'est un coin *)
                    if (my_mod y x) = 0 then
                        (* Obtenir le numéro du coin de 0 (pour (x, 0)) 
                            à 5 (pour (x, x * 6 - 1)) *)
                        let corner = y / x in
                        (* Retourner le tableau des voisins 
                        (partant du Nord et dans le sens des aiguilles d'une montre) *)
                        (x + 1, my_mod (y + corner - 1) ((x + 1) * 6))
                        ::(x + 1, y + corner)
                        ::(x + 1, my_mod (y + corner + 1) ((x + 1) * 6))
                        ::(x, my_mod (y + 1) (x * 6))
                        ::(x - 1, y - corner)
                        ::(x, my_mod (y - 1) (x * 6))
                        ::[]
                    else
                        (* Si c'est une arête *)

                        (* Fonction qui retourne le coin inférieur le plus proche de l'arête *)
                        let rec get_edge_num = fun x -> fun y ->
                            if (my_mod y x) = 0 then
                                y / x
                            else
                                get_edge_num x (y - 1)
                        in

                        (* Obtenir le numéro du coin le plus proche de 0 à 5 *)
                        let edge = get_edge_num x y in

                        (* Retourner le tableau des voisins 
                        (partant du Nord et dans le sens des aiguilles d'une montre) *)
                        (x + 1, y + edge)
                        ::(x + 1, y + edge + 1)
                        ::(x, my_mod (y + 1) (x * 6))
                        ::(x - 1, my_mod (y - edge) ((x - 1) * 6))
                        ::(x - 1, my_mod (y - edge - 1) ((x - 1) * 6))
                        ::(x, my_mod (y - 1) (x * 6))
                        ::[]
            ) in

            (* Vérifier la validité de chaque voisin, ne conserver que les valides *)
            List.fold_left 
                (fun acc ngb -> 
                    if (is_valid r ngb) then 
                        acc @ [ngb]
                    else
                        acc
                ) 
                [] neighbours
;;

(* Retourne une grille de rayon r *)
let create_grid = fun r ->
    (* Ajout des cases *)
    let lc = range 0 (r - 1) in
    let acc = List.fold_left 
        (fun acc x -> 
            List.fold_left 
                (fun acc2 y -> add_vertex (x,y) acc2) 
                acc (range 0 ((x * 6) - 1))
        ) 
        CellMap.empty lc
    in 
    
    (* Fonction qui lie c à ses voisins valides *)
    let add_neighbours_edges = fun c -> fun g ->
        List.fold_left
            (fun acc i -> add_edges c i acc)
            g (get_neighbours r c)

    in
    
    (* Fonction qui lie chaque case à ses voisins valides *)
    let rec set_edges = fun lc -> fun g ->
        match lc with
            | [] -> g
            | (k,_)::t -> set_edges t (add_neighbours_edges k g)

    in 
    
    (* Ajout des voisins *)
    set_edges (CellMap.bindings acc) acc
;;

(* Retourne un tuple (x,y) des cordonnées du centre de la case c *)
let rec get_center_coord = fun c ->
    (* Fonction qui retourne le coin inférieur le plus proche de l'arête *)
    let rec get_edge_num = fun x -> fun y ->
        if (my_mod y x) = 0 then
            y / x
        else
            get_edge_num x (y - 1)
    in

    match c with
        | (x, y) ->
            (* (0, 0) est un schéma unique *)
            if x = 0 && y = 0 then
                (0, 0)
            else
                (* Si c'est un coin *)
                if (my_mod y x) = 0 then
                    (* Obtenir le numéro du coin de 0 (pour (x, 0)) 
                        à 5 (pour (x, x * 6 - 1)) *)
                        let corner = y / x in
                        (* Positions x et y respectives des coins (1, 0) à (1, 5) *)
                        let x_distance = 0::7::7::0::(-7)::(-7)::[] in
                        let y_distance = 8::4::(-4)::(-8)::(-4)::4::[] in
                        (* Il suffit de multiplier x et y par le numéro de couronne (ici x) de la case 
                        pour obtenir le placement de n'importe quel coin *)
                        ((List.nth x_distance corner) * x, (List.nth y_distance corner) * x)
                else
                    (* Si c'est une arête *)

                    (* Obtenir le numéro du coin le plus proche de 0 à 5 *)
                    let edge = get_edge_num x y in
                    (* Obtenir la position du coin inférieur le plus proche *)
                    let ex1, ey1 = get_center_coord (x, edge * x) in
                    (* Obtenir la position du coin supérieur le plus proche *)
                    let ex2, ey2 = get_center_coord (x, (my_mod ((edge + 1) * x) (x * 6))) in
                    (* Calculer les distances entre les deux coins et diviser par la couronne
                    pour obtenir la valeur d'une "marche". Ajouter 1 marche à la position
                    du coin inférieur permet d'obtenir la position de la première case du
                    côté. *)
                    let step_x = (ex2 - ex1) / x in
                    let step_y = (ey2 - ey1) / x in
                    (* Obtenir la position de la case dans le côté pour savoir combien
                    de "marches" il faut ajouter *)
                    let pos_in_edge = my_mod y x in
                    (* Ajouter, à la position du point inférieur, autant de "marches"
                    que nécessaire *)
                    (ex1 + step_x * pos_in_edge, ey1 + step_y * pos_in_edge)
;;

(* Retourne une liste des cordonnées des 6 coins de la case c *)
let get_contour = fun c ->
    match (get_center_coord c) with
        | (x, y) -> (x - 2, y + 4)::(x + 2, y + 4)::(x + 5, y)::(x + 2, y - 4)
                    ::(x - 2, y - 4)::(x - 5, y)::[]
;;

(* Retourne une liste des cordonnées des coins communs aux cases c1 et c2 *)
let get_wall = fun c1 -> fun c2 ->
    (* Obtenir les contours des 2 cases *)
    let contour1 = get_contour c1 in
    let contour2 = get_contour c2 in

    (* Fonction qui retourne les positions des 2 points qui forment une face
    Les faces sont vont de 0 à 5 *)
    let get_face_coord = fun contour -> fun face ->
        (List.nth contour face)::(List.nth contour ((face + 1) mod 6))::[]
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

    (* Pour chaque faces de 0 à 5 de la case 1, la comparer avec sa face oposée de la case 2
    Si ce sont les mêmes, c'est que les faces sont "collées" *)
    List.fold_left
        (fun acc f -> 
            let f1 = get_face_coord contour1 f in
            if ( compare_faces f1 (get_face_coord contour2 ((f + 3) mod 6)) ) then
                f1
            else
                acc
        )
        [] (range 0 5)
;;

(* Retourne le voisin correspondant à la touche appuyée, qu'il soit valide ou non
Retourne (1, -1) pour toutes touches autres que a, z, e, q, s et d *)
let key_pressed = fun k -> fun (x, y) ->
    (* L'index représente le numéro du coin ou de l'arête du la case actuelle
    Il est important de le connaitre car les voisins "pivotent" plus ou moins
    autour de la case en fonction de sa position *)
    let index = (
        if x = 0 && y = 0 then 
            0
        else
            if (my_mod y x) = 0 then
                y / x
            else
                let rec get_edge_num = fun x -> fun y ->
                    if (my_mod y x) = 0 then
                        y / x
                    else
                        get_edge_num x (y - 1)
                in
    
                get_edge_num x y
    ) in

    (* On récupère les voisins (valides ou non) et on utilise shift et index
    pour simuler le "pivotement" de ceux-ci *)
    let neighbours = shift (
        (* (0, 0) est un schéma unique *)
        if x = 0 && y = 0 then 
            (1, 5)::(1, 0)::(1, 1)::(1, 4)::(1, 3)::(1, 2)::[]
        else
            (* Si c'est un coin *)
            if (my_mod y x) = 0 then
                (* Retourner le tableau des voisins 
                (partant du Nord et dans le sens des aiguilles d'une montre) *)
                (x + 1, my_mod (y + index - 1) ((x + 1) * 6))
                ::(x + 1, y + index)
                ::(x + 1, my_mod (y + index + 1) ((x + 1) * 6))
                ::(x, my_mod (y + 1) (x * 6))
                ::(x - 1, y - index)
                ::(x, my_mod (y - 1) (x * 6))
                ::[]
            else
                (* Si c'est une arête *)

                (* Retourner le tableau des voisins 
                (partant du Nord et dans le sens des aiguilles d'une montre) *)
                (x, my_mod (y - 1) (x * 6))
                ::(x + 1, y + index)
                ::(x + 1, y + index + 1)
                ::(x, my_mod (y + 1) (x * 6))
                ::(x - 1, my_mod (y - index) ((x - 1) * 6))
                ::(x - 1, my_mod (y - index - 1) ((x - 1) * 6))
                ::[]
    ) index in

    (* Retourne la case à la position propre à la touche appuyée
    (-1, -1) si la touche n'est pas reconnue *)
    match k with
        | 97 -> List.nth neighbours 0 (* a Nord-Ouest *)
        | 122 -> List.nth neighbours 1 (* z Nord *)
        | 101 -> List.nth neighbours 2 (* e Nord-Est *)
        | 113 -> List.nth neighbours 5 (* q Sud-Ouest *)
        | 115 -> List.nth neighbours 4 (* s Sud *)
        | 100 -> List.nth neighbours 3 (* d Sud-Est *)
        | _ -> ((-1), (-1)) (* Autres *)
;;