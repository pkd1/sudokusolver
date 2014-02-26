

(* REPRESENTATION CONVENTION:
   REPRESENTATION INVARIANT:
*)

abstype board = Board of int * int list vector
with
    fun emptyBoard (boardside : int) =
        let
            val l = List.tabulate (boardside, (fn x => x+1))
        in
            Board (boardside, Vector.tabulate ((boardside*boardside),
                                               (fn _ => l)))
        end

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
        if 0 <= index andalso index < boardside*boardside
        then (index mod boardside, index div boardside)
        else raise Subscript

    fun getCell (Board (boardside, vec) : board) (x : int) (y : int) =
        Vector.sub(vec, xyToIndex boardside x y)

    exception NotASolution

    fun setCell (Board (boardside, vec) : board) (x : int) (y : int) (num : int) =
        let
            val newvec =
                Vector.mapi (fn (index, poss) =>
                                let
                                    val (xi, yi) = indexToxy boardside index
                                    val b = xyToBlock boardside x y
                                    val bi = (case indexToxy boardside index of
                                                  (xi,yi) => xyToBlock boardside xi yi)
                                in
                                    case (xi = x, yi = y, bi = b) of
                                        (true,true,_)       => [num]                  (* The cell being updated *)
                                      | (false,false,false) => Vector.sub(vec, index) (* other block, column and row *)
                                      (* not the cell being updated but on a common block, column or row. *)
                                      | (_,_,_) => List.filter (fn x => x <> num) poss
                                end)
                            vec

            (* Find the new singleton lists. Panic on nil *)
            fun singleton_coordinates (v: int list vector)
              = Vector.foldli (fn (i,l,res)
                                  => case (l,i=xyToIndex boardside x y) of
                                         (* In case of a singleton, note the xy-coord and the member... *)
                                         (a::nil,false) => (case Vector.sub(vec,i) of
                                                                b::c::t => (indexToxy boardside i, a)::res
                                                              | _       => res)
                                       (* ...and panic if encountering nil. *)
                                       | (nil,_) => raise NotASolution
                                       | _ => res)
                              [] v
        in
            (* Update all of the changed positions using setCell to propagate the new restrictions. *)
            List.foldl (fn ( ((x,y), a), brd)
                           => setCell (Board(boardside, newvec)) x y a)
                       (Board(boardside, newvec))
                       (singleton_coordinates newvec)
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

fun singeltonToOption [x] = SOME x
  | singeltonToOption _ = NONE

fun vecToList v = Vector.foldl (fn (e,buf) => e::buf) [] v

fun extract (vec, start, NONE) = vec (* not implemented *)
  | extract (vec, start, SOME nElements) =
    let
        fun toList 0 = []
          | toList n = (Vector.sub(vec,start+n-1))::(toList (n-1))
    in
        (PolyML.print (toList nElements); Vector.fromList(toList nElements))
    end

(* split l
   TYPE: a' list -> a' list
   PRE:  true ??????????????????????????????????????
   POST: a list split in in several lists of chunkSize elements.
   VARIANT: length l
   EXCEPTION: raises:
      MalformattedBoard - if chunkSize is not a dividier in length l.
*)

fun split chunkSize [] = []
  | split chunkSize l = (List.take(l,chunkSize))::
                         (split chunkSize (List.drop(l,chunkSize))
                          handle Subscript => raise MalformattedBoard)
(*
fun split 0 bs vec = []
  | split endElement bs vec = (extract(vec, endElement-bs, SOME bs))::(split (endElement-bs) bs vec)
*)
fun vecToIntOptionListList bs vec =
    split bs (vecToList (Vector.map singeltonToOption vec))

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
                   | (SOME n, (b,x)) => (setCell b x y n,x-1))
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
