

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

(* readNumbersFromLine line
   TYPE: string -> int option list
   PRE:  line is a string with integers or empty strings or white space
         characters seperated by commas.
   POST: a list with SOME int:s where int is the same integer in the same 
         position as in line, with empty strings or whitespaces between
         commas represented by NONE.
   EXAMPLE: readNumbersFromLine "1, ,0,4,,3," =
            [SOME 1, NONE, SOME 0, SOME 4, NONE, SOME 3, NONE]
*)
fun readNumbersFromLine line = List.map Int.fromString (String.fields (fn c => c = #",") line)

(* readBoard stringlist
   TYPE: string list -> board
   PRE:  stringlist is a string representation of a board where every element
         is a string with integers, empty strings or white space characters
         seperated by commas.
         The number of commas in every element of stringlist must be one less
         then the length of stringlist.
   POST: a board with stringlist read in.
   EXAMPLE: readBoard ["2,3,4,1","4,1,2,3","3,4,1,2","1,2,3,4"] = Board
   EXCEPTIONS: raises:
      MalformattedBoard - if stringlist is malformatted.
*)
fun readBoard stringlist = raise Fail "Not Implemented"

(* readBoardsFile boardsFile
   TYPE: string -> board list
   PRE:  boardsFile is a valid path to a valid readable boards file.
   POST: a list containing the boards from the boardsFile
   EXAMPLE: readBoardsFile "example.boards" = [Board ?, ...]
   EXCEPTIONS: raises:
     MalformattedBoard - if boardsFile contains malformatted boards,
     Io                - if boardsFile could not be opened.
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
