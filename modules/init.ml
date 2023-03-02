(* Initialisations communes : structures, Random *)

(* Initialisation de Random *)
Random.self_init ();;

(* Représentation d'une case *)
module Cell = 
    struct 
        type t = int * int
        let compare = Pervasives.compare
    end
;;

(* Représentation d'une grille *)
module CellMap = Map.Make(Cell);;
module CellSet = Set.Make(Cell);;
type grid = CellSet.t CellMap.t;;