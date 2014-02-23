

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
        fun blockposToIndex (boardside : int) (block : int) (pos : int) : int =
            if 0 <= block andalso block < boardside andalso
               0 <= pos andalso pos < boardside then
                (block div 3) * boardside * 3 + ((block mod 3) * (boardside div 3)) + (pos div 3) * boardside + (pos mod 3)
            else raise Subscript
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

(* readLines fname
   TYPE: string -> string list
   PRE: fname must be the name of an existing (readable) file
   POST: a list of the lines that comprise the file fname
   Taken from 29-Side-Effects-and-IO.sml
 *)
fun readLines fname =
  let
    open TextIO
    (* readLinesAux istrm
       TYPE: instream -> string list
       PRE: true
       POST: a list of the lines in istrm
       SIDE-EFFECTS: closes istrm
     *)
    (* VARIANT: remaining lines in istrm *)
    fun readLinesAux istrm =
      case inputLine istrm of
        NONE => (closeIn istrm; [])
      | SOME line => line :: readLinesAux istrm
  in
    readLinesAux (openIn fname)
  end;

(* readBoardsFile boardsFile
   TYPE: string -> board list
   PRE:  boardsFile is a valid path to a valid readable boards file.
   POST: a list containing the boards from the boardsFile
   EXAMPLE: readBoardsFile "example.boards" = [Board ?, ...]
   EXCEPTIONS: raises:
     MalformattedBoardsFile - if boardsFile malformatted,
     FileNotFound           - if boardsFile not found.
*)
fun readBoardsFile boardsFile = raise Fail "Not implemented."


(* funktionsnamn argument
   TYPE: 
   PRE:  
   POST: 
   EXAMPLE: 
   SIDE-EFFECTS: 
   EXCEPTIONS: 
*)
