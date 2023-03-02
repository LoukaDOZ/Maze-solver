(* Fonctions communes utilitaires *)

(* Modulo qui accepte les x négatifs ce que l'opérateur mod d'Ocaml ne fait pas :
-1 mod m retourne toujours -1
Code pris de : https://stackoverflow.com/questions/46758683/ocaml-mod-function-returns-different-result-compared-with *)
let my_mod = fun x -> fun y ->
    let result = x mod y in
    if 
        result >= 0 then result
    else 
        result + y
;;

(* Fonction auxiliaire de shift *)
let rec shift_aux = fun l -> fun head -> fun tail -> fun i -> fun n ->
    match l with
      | [] -> tail @ head
      | h::t -> 
        if i < n then
            shift_aux t head (tail @ [h]) (i - 1) n
        else
            shift_aux t (head @ [h]) tail (i - 1) n
;;

(* Déplace les n premiers éléments à la fin de la liste
* Ne change pas l'ordre des éléments
* Exemple : shift [0;1;2;3] 2 -> [2;3;0;1] *)
let shift = fun l -> fun n ->
    shift_aux l [] [] ((List.length l) - 1) n
;;