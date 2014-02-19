

(* REPRESENTATION CONVENTION: 
   REPRESENTATION INVARIANT:  
*)



abstype board = Board of int * int list vector with
    fun emptyBoard (boardside : int) = 
        Board (boardside, Vector.tabulate ((boardside*boardside),
            (fn x => List.tabulate (boardside,
                (fn x => x+1)))))
    fun debug (Board (_, vec)) = vec
    fun setCell (Board (boardside, vec) : board) (x : int) (y : int) (possibilities : int list) =
        Board( boardside, Vector.update(vec, (y * boardside) + (x mod boardside), possibilities ))

(*
  fun get = ... *)
end

(* 
   
*)
