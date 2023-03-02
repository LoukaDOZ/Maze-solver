(* Fonctions communes de travail sur les listes : création, manipulations *)

(* Construit une liste d'entiers sur la base de 2 entiers a et b, a <= b : [a;a+1;...;b] *)
let rec range = fun a -> fun b ->
  if a >= b then
      [a]
  else
      [a] @ range (a + 1) b
;;

(* Construit une liste de tous les tuples (i, j) où 0 <= i < m et 0 <= j < n *)
let range2 = fun m -> fun n ->    
  List.fold_left 
      (fun acc i ->
          acc @ ( List.map ( fun j -> (i, j) ) (range 0 (n - 1)) )
      )
      [] (range 0 (m - 1))
;;

(* Retire le ième élément de la liste d'entiers l et renvoie la nouvelle liste *)
let remove_nth = fun l -> fun x ->
  List.fold_left 
      (fun acc i ->
          if x != i then
              acc @ [( List.nth l i )]
          else
              acc
      )
      [] ( range 0 ((List.length l) - 1) )
;;

(* Retire un élément aléatoire de la liste d'entiers l et retourne une tuple (x, r) :
x l'élément retiré et r la liste l privée de x *)
let extract_random = fun l ->
  let rand = Random.int (List.length l) in
  let new_list = remove_nth l rand in
  ( (List.nth l rand), new_list )
;;

(* Mélange une liste d'entiers l et retourne la liste mélangée 
qui contient tous les éléments de l dans un ordre aléatoire *)
let rec suffle = fun l ->
  if (List.length l) = 0 then
      []
  else
      let x,r = extract_random l in
      let r' = suffle(r) in
      x::r'
;;