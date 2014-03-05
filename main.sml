
exception MalformattedBoard

(* REPRESENTATION CONVENTION:
    Let b = Board(n,v).
    n stands for the length of the side of a box in the sudoku.
    This means that for
    - a normal 9x9,  n = 3
    - a        16x16,n = 4
    - a        4x4,  n = 2
    and so on. In general the side of a box is the root of the
    side of the board. This means that only boards with sides
    of length n*n is allowed where n is a positive integer.

    v is a vector of length n^4, or boardside^2 where
    boardside = n^2. For brevity and clarity, let us
    write Vector.sub(w,i+j*n*n) as w(i,j).
    v(i,j) represents a set of non-excluded values
    for the cell (i,j) in the sudoku
    (using indexes starting at zero) as a list of the
    non-excluded values.

    If v(i,j) = [k] the cell is considered to be set.
    k is said to be an impossibility at (i,j) if
      putting k at (i,j) makes the board impossible
      to solve.

    A board B' = Board (n',v') is said to extend B
    if v'(i,j) is a subset or equal to v(i,j).

   REPRESENTATION INVARIANT:

    Example of boxes in a 4x4 sudoku:
    12|34
    34|12
    -- --
    23|41
    41|23

    v(i,j) is a subset of {1,...,n*n}
    v(i,j) is not empty

    If (a,b) \neq (i,j), v(a,b) = [k] and one
    of the below holds then k \notin v(i,j).
    - (i,j) is in the same box as (a,b)
    - a = i
    - b = j


*)

abstype board = Board of int * int list vector
with
    fun emptyBoard (boardSide : int) =
        let
            val sq = trunc (Math.sqrt (real boardSide))
            val boxSide = if sq*sq = boardSide then sq else
                          raise MalformattedBoard
            val l = List.tabulate (boardSide, (fn x => x+1))
        in
            Board (boxSide, Vector.tabulate ((boardSide*boardSide),
                                               (fn _ => l)))
        end

    fun debug (Board (_,vec)) = vec (* only used in test suite *)
    fun getBoxSide (Board(bs,_)) = bs
    fun getBoardSide (Board(bs,_)) = bs*bs
    fun getBoardSize (Board(bs,_)) = let val b = bs*bs in b*b end

    fun xyToBlock (boxSide : int) (x : int) (y : int) =
        (y div boxSide) * boxSide + x div boxSide

    fun xyToIndex (boardside : int) (x : int) (y : int) : int =
        if 0 <= x andalso x < boardside andalso
           0 <= y andalso y < boardside then
            (y * boardside) + x
        else raise Subscript
    fun indexToxy (boardside : int) (index : int) =
        if 0 <= index andalso index < boardside*boardside
        then (index mod boardside, index div boardside)
        else raise Subscript

    fun getCell (Board (boxSide, vec) : board) (x : int) (y : int) =
        Vector.sub(vec, xyToIndex (boxSide*boxSide) x y)

    fun boardToListList b =
        let
            val bs = getBoardSide b
            fun revBoardToListList row full 0 0 =
                (rev ((getCell b 0 0)::row))::full
              | revBoardToListList row full 0 y =
                revBoardToListList [] (rev ((getCell b 0 y)::row)::full) (bs-1) (y-1)
              | revBoardToListList row full x y =
                revBoardToListList ((getCell b x y)::row) full (x-1) y
        in
            rev (revBoardToListList [] [] (bs-1) (bs-1))
        end

    exception NotASolution

    (* setCell B x y value
       TYPE: board -> int -> int -> int -> board
       PRE:  1 <= x, y, value <= boardside
       POST: Let B = Board(boxside,v), (a,b) \neq (x,y),
                 B' = setCell B x y value and
                 B' = Board(boardside',v').

             We use the same terminology and notation as in
             the representation convention.

             - v(i,j) \setminus v'(i,j) is a set containing
               only impossibilities at (i,j).
             - v'(a,b) is a subset or is equal to v(a,b).
             - If (i,j) is distinct from (x,y) but in the
               same row, column or box then value is not
               a member of v(i,j).

             If v'(i,j) is found to be inconsistent then
             the NotASolution exception is raised
             exception is raised.
       EXAMPLE: setCell (emptyBoard 4) 0 0 1 =
                 Board(2, fromList
                           [[4],      [1, 2, 3],   [1, 2, 3],   [1, 2, 3],
                            [1, 2, 3],[1, 2, 3],   [1, 2, 3, 4],[1, 2, 3, 4],
                            [1, 2, 3],[1, 2, 3, 4],[1, 2, 3, 4],[1, 2, 3, 4],
                            [1, 2, 3],[1, 2, 3, 4],[1, 2, 3, 4],[1, 2, 3, 4]])
       EXCEPTIONS: may raise NotASolution.
       VARIANT: The sum of the lengths of the lists in v.
     *)
    fun setCell (oldbrd as Board (oldbs, oldvec) : board) (x : int) (y : int) (value : int) =
        let
            val boardside = getBoardSide oldbrd
            val boxSide = getBoxSide oldbrd
            fun removeValueFromRowColBlock (index, possibilities_at_i) =
                let
                    val (xi, yi) = indexToxy boardside index
                    val block_of_xy = xyToBlock boxSide x y
                    val block_of_i  = xyToBlock boxSide xi yi
                in
                    case (xi = x, yi = y, block_of_i = block_of_xy) of
                        (true,true,_)       => [value]                  (* The cell being updated *)
                      | (false,false,false) => Vector.sub(oldvec, index) (* other block, column and row *)
                      (* not the cell being updated but on a common block, column or row. *)
                      | (_,_,_) => List.filter (fn x => x <> value) possibilities_at_i
                end
            val newvec = Vector.mapi removeValueFromRowColBlock oldvec

            (* Find the new singleton lists. Panic on nil *)
            fun singleton_coordinates (v: int list vector)
              = Vector.foldli (fn (index,possibilities_at_index,accumulator)
                                  => case (possibilities_at_index,index=xyToIndex boardside x y) of
                                         (* In case of a singleton, note the xy-coord and the member... *)
                                         (a::nil,false) => (case Vector.sub(oldvec,index) of
                                                                b::c::t => (indexToxy boardside index, a)::accumulator
                                                              | _       => accumulator)
                                       (* ...and panic if encountering nil. *)
                                       | (nil,_) => raise NotASolution
                                       | _ => accumulator)
                              [] v

                fun propagate_at_xy( ((x,y),value_at_xy), brd )
                    = setCell brd x y value_at_xy

        in
            (* Update all of the changed positions using setCell to propagate the new restrictions. *)
            List.foldl propagate_at_xy
                       (Board(oldbs, newvec))
                       (singleton_coordinates newvec)
        end



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





fun singeltonToString [x] = StringCvt.padLeft #" " 8 (Int.toString x)
  | singeltonToString _ = "        "

fun listToString pad l = "[" ^ (StringCvt.padLeft #" " (pad*2-1)
           (String.concatWith "," (List.map Int.toString l))) ^ "]"

fun toFString f b =
    let
        val l = boardToListList b
        fun concatWith string func = String.concatWith string o List.map func;
    in
       concatWith "\n" (concatWith "," f) l
    end

val toString = toFString singeltonToString

val printBoard = print o toString

fun printPos b =
    let
        val len = getBoardSide b
    in
        (print o toFString (listToString len)) b
    end

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
                 (readBoardAux (y-1) xs,lsl-1) (readNumbersFromLine s)
                  handle Subscript => raise MalformattedBoard) (* Wrong dim *)
          | readBoardAux _ _ = raise MalformattedBoard
    in
        readBoardAux (lsl-1) (rev stringlist)
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
    let
        val fileStrings = readLines boardsFile
        fun readBF boards [] [] = boards
          | readBF boards buf [] = buf::boards
          | readBF boards [] (("\n")::lines) = readBF boards [] lines
          | readBF boards buf (("\n")::lines) =
                          readBF (buf::boards) [] lines
          | readBF boards buf (currentLine::lines) =
                          readBF boards (currentLine::buf) lines
    in
        List.map readBoard (readBF [] [] (rev fileStrings))
    end

(* funktionsnamn argument
   TYPE:
   PRE:
   POST:
   EXAMPLE:
   SIDE-EFFECTS:
   EXCEPTIONS:
   VARIANT:
*)
