

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
        fun boxSide boardside =
            let
                val sq = trunc (Math.sqrt (real boardside))
            in
                if sq*sq = boardside then sq else 1
            end

        fun xyToBlock (boardside : int) (x : int) (y : int) =
            let
                val bs = boxSide boardside
            in
                (y div bs) * bs + x div bs
            end
        fun blockposToIndex (boardside : int) (block : int) (pos : int) : int =
            if 0 <= block andalso block < boardside andalso
               0 <= pos andalso pos < boardside then
                let
                    val sqrtSide = boxSide boardside
                in
                    (block div sqrtSide) * boardside * sqrtSide
                    + ((block mod sqrtSide) * sqrtSide)
                    + (pos div sqrtSide) * boardside
                    + (pos mod sqrtSide)
                end
            else raise Subscript
        fun xyToIndex (boardside : int) (x : int) (y : int) : int =
            if 0 <= x andalso x < boardside andalso
               0 <= y andalso y < boardside then
                (y * boardside) + x
            else raise Subscript
	fun indexToxy (boardside : int) (index : int) =
	    if 0 <= index andalso index <= boardside*boardside
	    then (index mod boardside, index div boardside)
	    else raise Subscript

        fun getCell (Board (boardside, vec) : board) (x : int) (y : int) =
                Vector.sub(vec, xyToIndex boardside x y)

	exception NotASolution

        fun setCell (Board (boardside, vec) : board) (x : int) (y : int) (num : int) =
            Board( boardside, Vector.update(vec, xyToIndex boardside x y, possibilities ))

	let
	    val newBoard =
		Vector.mapi (fn (index, poss) =>
				let
				    val (xi, yi) = indexToxy boardside index
				    val bi = indexToBlock index
				    val 
				in
				    case (xi = x, yi = y, bi = b) of
					(true,true,_   ) => [num]                     (* The cell being updated *)
				      | (false,false,false) => Vector.sub(vec, index) (* other block, column and row *)
				      (* other block, column and row *)
				      | (_,_,_) => filter (fn x => x <> num) (Vector.sub(vec, index))              
				end
	    (* Finds the indexes of interest  *)
	    fun ioi(v:Vector) = Vector.foldli (fn (i,l,res) => case (l,i=xyToIndex boardsize x y) of 
							     (a::nil,false) => case Vector.sub(vec,i) of
										   b::c::t => i::res
										 | _ => res
							   | (nil,_) => raise NotASolution (* or something*)
							   | (_,_)   => res)
					      [] v
	in
	    List.foldl (fn index => ) (* Trasigt... *)
	    
	end




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

exception MalformattedBoard

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
fun readBoard [] = raise Fail "No data"
  | readBoard stringlist =
    let
        val lsl = length stringlist
        fun readBoardAux ~1 _ = emptyBoard lsl
          | readBoardAux y (s::xs) =
             (fn (bb,~1) => bb
               | _ => raise MalformattedBoard) (* Wrong dim *)
                 (List.foldr
                 (fn ( NONE , (b,x)) => (b,x-1)
                   | (SOME n, (b,x)) => (setCell b x y [n],x-1))
                 (readBoardAux (y-1) xs,lsl-1) (readNumbersFromLine s))
            handle Subscript => raise MalformattedBoard (* Wrong dim *)
    in
        readBoardAux (lsl-1) stringlist
    end

(* readBoardsFile boardsFile
   TYPE: string -> board list
   PRE:  boardsFile is a valid path to a valid readable boards file.
   POST: a list containing the boards from the boardsFile
   EXAMPLE: readBoardsFile "example.boards" = [Board ?, ...]
   EXCEPTIONS: raises:
     MalformattedBoard - if boardsFile contains malformatted boards,
     Io                - if boardsFile could not be opened.
*)
fun readBoardsFile boardsFile =
    (fn (b,l) => List.map readBoard (l::b)) (
        List.foldl
        (fn ("\n",(buf,sl)) => (sl::buf,[])
          | (s,(buf,sl)) =>  (buf,s::sl) )
        ([],[]) (readLines boardsFile))

(* funktionsnamn argument
   TYPE: 
   PRE:  
   POST: 
   EXAMPLE: 
   SIDE-EFFECTS: 
   EXCEPTIONS: 
*)
