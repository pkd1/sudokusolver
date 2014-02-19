

(* REPRESENTATION CONVENTION: 
   REPRESENTATION INVARIANT:  
*)

abstype board = Board of int * int list vector
with
        fun emptyBoard (boardside : int) =
            Board (boardside, Vector.tabulate ((boardside*boardside),
                (fn x => List.tabulate (boardside,
                    (fn x => x+1)))))
        fun debug (Board (_, vec)) = vec
        (* local *)
        fun posToIndex (boardside : int) (x : int) (y : int) : int = (y * boardside) + (x mod boardside)
        fun getCell (Board (boardside, vec) : board) (x : int) (y : int) =
                Vector.sub(vec, posToIndex boardside x y)
        fun setCell (Board (boardside, vec) : board) (x : int) (y : int) (possibilities : int list) =
            Board( boardside, Vector.update(vec, posToIndex boardside x y, possibilities ))

        (* in *)
        (* end *)
end

(* 
   
*)
