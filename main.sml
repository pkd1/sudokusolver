
exception MalformattedBoard
exception NotASolution

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
    v(i,j) represents the set of non-excluded values
    for the cell (i,j) on the sudoku board
    (using indexes starting at zero) as a list of the
    non-excluded values. For ease of specification
    v(i,j) is sometimes regarded as the set it represents
    Sets are encoded in lists by enumerating each member
    in increasing.

    If v(i,j) = [k] the cell is considered to be set.
    k is said to be an impossibility at (i,j) if
      putting k at (i,j) makes the board impossible
      to solve.

    Vocabulary:
       Let B' = Board (n',v') below.

       B' is said to extend B if n=n' and v'(i,j) is a
       subset or equal to v(i,j) for all (i,j).

       B' is said to be a solution if all v'(i,j) are
       singletons.

       B' is called inconsistent if it cannot be extended
       to a solution.

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

    (* emptyBoard boardSide
       TYPE: int -> board
       PRE:  boardSide has an integer square root.
       POST: a board with possibilities 1 to boardSide in every cell.
       EXAMPLE: emptyBoard 4 = Board (2, fromList[[1, 2, 3, 4],
        [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4],
        [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4],
        [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4]])
       EXCEPTIONS: raises:
        MalformattedBoard - if boardSide does not have an integer square root.
    *)
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

    (* debug (Board (_,vec))
       TYPE: board -> int list vector
       PRE:  true
       POST: the vector of the board for debug and testing purpuses.
       EXAMPLE: debug (emptyBoard 4) = fromList[[1, 2, 3, 4],
        [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4],
        [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4],
        [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4]]
    *)
    fun debug (Board (_,vec)) = vec (* only used in test suite *)

    (* getBoxSide (Board (bs,_))
       TYPE: board -> int
       PRE:  true
       POST: the box side bs
       EXAMPLE: getBoxSide (emptyBoard 4) = 2
    *)
    fun getBoxSide (Board(bs,_)) = bs

    (* getBoardSide (Board (bs,_))
       TYPE: board -> int
       PRE:  true
       POST: the board side (bs*bs)
       EXAMPLE: getBoardSide (emptyBoard 4) = 4
    *)
    fun getBoardSide (Board(bs,_)) = bs*bs

    (* getBoardSize (Board (bs,_))
       TYPE: board -> int
       PRE:  true
       POST: the board size bs^4
       EXAMPLE: getBoardSize (emptyBoard 4) = 16
    *)
    fun getBoardSize (Board(bs,_)) = let val b = bs*bs in b*b end

    (* xyToBlock boxSide x y
       TYPE: int -> int -> int -> int
       PRE:  0 <= x < boxSide^2, 0 <= y < boxSide^2
       POST: the block of the cell at (x,y), blocks numbered as follows:
                 1|1||2|2
                 1|1||2|2
                 ========
                 3|3||4|4
                 3|3||4|4
       EXAMPLE: xyToBlock 2 2 2 = 3
       EXCEPTION: raises Subscript if x or y not in specified range.
    *)
    fun xyToBlock (boxSide : int) (x : int) (y : int) =
        if 0 <= x andalso x < boxSide*boxSide andalso
           0 <= y andalso y < boxSide*boxSide then
        (y div boxSide) * boxSide + x div boxSide
        else raise Subscript

    (* xyToIndex boardSide x y
       TYPE: int -> int -> int -> int
       PRE:  0 <= x < boardSide, 0 <= y < boardSide
       POST: the index in the vector of the cell at (x,y)
       EXAMPLE: xyToIndex 4 2 2 = 10
       EXCEPTION: raises Subscript if x or y not in specified range.
    *)
    fun xyToIndex (boardside : int) (x : int) (y : int) : int =
        if 0 <= x andalso x < boardside andalso
           0 <= y andalso y < boardside then
            (y * boardside) + x
        else raise Subscript

    (* xyToIndex boardSide index
       TYPE: int -> int -> int
       PRE:  0 <= index < boardSide*boardSide
       POST: the tuple with coords (x,y) corresponding to index of a vector
             representing a board with board side boardSide.
       EXAMPLE: indexToxy 4 10 = (2,2)
       EXCEPTION: raises Subscript if index not in specified range.
    *)
    fun indexToxy (boardside : int) (index : int) =
        if 0 <= index andalso index < boardside*boardside
        then (index mod boardside, index div boardside)
        else raise Subscript

    (* getCell (Board(boxSide,vec)) x y
       TYPE: board -> int -> int -> int list
       PRE:  0 <= x < boxSide^2, 0 <= y < boxSide^2
       POST: the int list at pos (indexOfxy boxSide^2) in vec.
       EXAMPLE: getCell (emptyBoard 4) 2 2 = [1,2,3,4]
       EXCEPTION: raises Subscript if x or y not in specified range.
    *)
    fun getCell (Board (boxSide, vec) : board) (x : int) (y : int) =
        Vector.sub(vec, xyToIndex (boxSide*boxSide) x y)

    (* boardToListList b
       TYPE: board -> int list list list
       PRE:  true
       POST: b as an int list list list representation.
       EXAMPLE: boardToListList (emptyBoard 4) =
          [[[1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4]],
           [[1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4]],
           [[1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4]],
           [[1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4]]]
    *)
    fun boardToListList b =
        let
            val bs = getBoardSide b
            (* revBoardToListList b x y
               TYPE: board -> int -> int -> int list list
               PRE:  0 <= x < bs, 0 <= y < bs
               POST: a list of lists of a (splitted at length bs) list of cells
                     (int lists), traversed backwards from (x,y) to (0,0)
               EXAMPLE: with bs = 4 and b = emptyBoard 4,
                  revBoardToListList [] [] 1 2 =
                   [[[1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4]],
                    [[1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4]]]
               EXCEPTION: raises Subscript if x or y not in specified range.
            *)
            fun revBoardToListList row full 0 0 =
                (rev ((getCell b 0 0)::row))::full
              | revBoardToListList row full 0 y =
                revBoardToListList [] (rev ((getCell b 0 y)::row)::full) (bs-1) (y-1)
              | revBoardToListList row full x y =
                revBoardToListList ((getCell b x y)::row) full (x-1) y
        in
            rev (revBoardToListList [] [] (bs-1) (bs-1))
        end

    (* setCell Board(boxside,v) x y value
       TYPE: board -> int -> int -> int -> board
       PRE:  1 <= x, y, value <= boxside^2 (* = boardside *)
       POST: Let (a,b) \neq (x,y),
                 B' = setCell B x y value and
                 B' = Board(boardside',v').

             The terminology and notation as in
             the representation convention is used.

             - v(i,j) \setminus v'(i,j) is a set containing
               only impossibilities at (i,j).
             - v'(a,b) is a subset or is equal to v(a,b).
             - If (i,j) is distinct from (x,y) but in the
               same row, column or box then value is not
               a member of v(i,j).

       EXAMPLE: setCell (emptyBoard 4) 0 0 1 =
                 Board(2, fromList
                           [[4],      [1, 2, 3],   [1, 2, 3],   [1, 2, 3],
                            [1, 2, 3],[1, 2, 3],   [1, 2, 3, 4],[1, 2, 3, 4],
                            [1, 2, 3],[1, 2, 3, 4],[1, 2, 3, 4],[1, 2, 3, 4],
                            [1, 2, 3],[1, 2, 3, 4],[1, 2, 3, 4],[1, 2, 3, 4]])
       EXCEPTIONS: If v'(i,j) is found to be inconsistent then
                   the NotASolution exception is raised
       VARIANT: The sum of the lengths of the lists in v.
     *)
    fun setCell (oldbrd as Board (oldbs, oldvec) : board)
                (x : int) (y : int) (value : int) =
        let
            val boardside = getBoardSide oldbrd
            val boxSide = getBoxSide oldbrd
            (* removeValueFromRowColBlock(index, possibilitiesAti)
             TYPE: int * int list -> board
             PRE:  1 <= i <= oldbs^4 (* = boardside^2 *)
             POST: Let (xi,yi) = indexToxy index
                   Board(oldbs, v') where
                   - v'(x,y) = [value]
                   - if (xi,yi) distinct from (x,y) but is in
                     the same line, column or row as (x,y)
                     then v'(xi,yi)
                             = possibilitiesAti \setminus {value}

                   - if (xi,yi) is not in the same row, column
                     or box as (x,y) then v'(xi,yi) = possibilitiesAti
             EXAMPLE: removeValueFromRowColBlock index
             *)

            fun removeValueFromRowColBlock (index, possibilitiesAti) =
                let
                    val (xi, yi) = indexToxy boardside index
                    val blockOfxy = xyToBlock boxSide x y
                    val blockOfi  = xyToBlock boxSide xi yi
                in
                    case (xi = x, yi = y, blockOfi = blockOfxy) of
                        (* The cell being updated *)
                        (true,true,_)       => [value]
                        (* Other block, column and row *)
                      | (false,false,false) => Vector.sub(oldvec, index)
                        (* Not the cell being updated but on a
                             common block, column or row. *)
                      | (_,_,_) => List.filter (fn x => x <> value)
                                               possibilitiesAti
                end
            val newvec = Vector.mapi removeValueFromRowColBlock oldvec

            (* getValueAndCoordinateOfSingletons vec
             TYPE: int list vector -> (int*int)*int
             PRE:  The length of vec is oldbs^4 (* = boardside^2 *)
             POST: Finds all of the cells that have turned into
                   singletons between oldvec and vec and returns a
                   list of the (coordinate,value in the
                   singleton) of those cells.
             EXAMPLE: Let oldvec be Vector.fromList
                           [[1], [2], [4],    [3],
                            [4], [3], [1, 2], [1, 2],
                            [2], [1], [3],    [4],
                            [3], [4], [1, 2], [1, 2]])
                      getValueAndCoordinateOfSingletons
                        Vector.fromList [[1], [2], [4], [3],
                                         [4], [3], [1], [2],
                                         [2], [1], [3], [4],
                                         [3], [4], [2], [1, 2]])
                       = [((2,3), 2),((3,1), 2)]
             *)
            fun getValueAndCoordinateOfSingletons (v: int list vector)
              = Vector.foldli
                    (fn (index,possibilitiesAtIndex,accumulator)
                        => case (possibilitiesAtIndex,
                                 index=xyToIndex boardside x y) of
                               (* In case of a singleton,
                                    put the xy-coord and the member
                                    into the accumulator... *)
                               (a::nil,false)
                                 => (case Vector.sub(oldvec,index) of
                                         b::c::t
                                           => (indexToxy boardside index, a)
                                                 ::accumulator
                                       | _ => accumulator)
                               (* ...and panic if encountering nil. *)
                             | (nil,_) => raise NotASolution
                             | _ => accumulator)
                    [] v

        in
            (* Update all of the changed positions using setCell
                 to propagate the new restrictions. *)
            List.foldl (fn (((x,y),valueAtxy), brd )
                           => setCell brd x y valueAtxy)
                       (Board(oldbs, newvec))
                       (getValueAndCoordinateOfSingletons newvec)
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
fun readNumbersFromLine line
      = List.map Int.fromString (String.fields (fn c => c = #",") line)

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

(* singeltonToString l
   TYPE: int list -> string
   PRE:  true
   POST: the value of the singelton as a string padded to length 2 with spaces
         if l is a singelton, otherwise a string with 2 spaces.
   EXAMPLE: singeltonToString [2,3,4] = "  ";
            singeltonToString [4] = " 4";
            singeltonToString [] = "  ";
*)
fun singeltonToString [x] = StringCvt.padLeft #" " 2 (Int.toString x)
  | singeltonToString _ = "  "

(* listToString pad l
   TYPE: int -> int list -> string
   PRE:  true
   POST: a string representation of l padded to length 2*pad-1 with spaces,
         surrounded by square brackets.
   EXAMPLE: listToString 2 [2,3,4] = "[2,3,4]";
            listToString 3 [4] = "[    4]";
            listToString 2 [] = "[   ]";
*)
fun listToString pad l = "[" ^ (StringCvt.padLeft #" " (pad*2-1)
           (String.concatWith "," (List.map Int.toString l))) ^ "]"


(* toFString f b
   TYPE: (int list -> string) -> board -> string
   PRE:  true
   POST: a string representation of b, with every cell represented by f.
   EXAMPLE: toFString (fn l => Int.toString (length l)) (emptyBoard 4) =
            "4,4,4,4\n4,4,4,4\n4,4,4,4\n4,4,4,4";
*)
fun toFString f b =
    let
        val l = boardToListList b

        (* concatWith s f l
           TYPE: string -> ('a -> string) -> 'a list -> string
           PRE:  true
           POST: a string representation of l, with every element represented
                 by f, concatenated with s.
           EXAMPLE: concatWith "." (str o Char.toUpper o hd o explode)
                               ["Magnetic","resonance","imageing"] =
                    "M.R.I";
        *)
        fun concatWith string func = String.concatWith string o List.map func;
    in
       concatWith "\n" (concatWith "," f) l
    end

val toString = toFString singeltonToString

val printBoard = print o toString

(* printPos b
   TYPE: board -> unit
   PRE:  true
   POST: a string representation of b, with every cell represented by f.
   EXAMPLE: printPos (readBoard ["1,,2,",",3,,","4,,,",",,,"]);
   SIDE-EFFECTS: Prints the string representation of b with possibilities.
   [  1,2,4],[    1,4],[    1,2],[      3]
   [    1,2],[    1,3],[    1,2],[      4]
   [    1,4],[    1,4],[      3],[      2]
   [      3],[      2],[      4],[      1]
*)
fun printPos b = (print o toFString (listToString (getBoardSide b))) b

(* funktionsnamn argument
   TYPE:
   PRE:
   POST:
   EXAMPLE:
   SIDE-EFFECTS:
   EXCEPTIONS:
   VARIANT:
*)
