

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
        fun xyToIndex (boardside : int) (x : int) (y : int) : int =
            if 0 <= x andalso x < boardside andalso
               0 <= y andalso y < boardside then
                (y * boardside) + x
            else raise Subscript
        fun getCell (Board (boardside, vec) : board) (x : int) (y : int) =
                Vector.sub(vec, xyToIndex boardside x y)
        fun setCell (Board (boardside, vec) : board) (x : int) (y : int) (possibilities : int list) =
            Board( boardside, Vector.update(vec, xyToIndex boardside x y, possibilities ))

        (* in *)
        (* end *)
end

(* 
   
*)
