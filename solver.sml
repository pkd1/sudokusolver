use "main.sml";

fun findFirstSolution(brd: board) : board option =
    let
        val boardside = getBoardSide brd
        val coordinates = List.tabulate(boardside, fn n => n)
        fun smallerthan(newList: int list, oldList : int list) =
            let
                val newLen = List.length newList
                val oldLen = List.length oldList
            in
                newLen >= 2 andalso (oldLen = 1 orelse newLen < oldLen)
            end

        val coordinateOfSmallestList =
            List.foldl (fn (x,minimalSoFar) =>
                           List.foldl
                               (fn (y,(oldx,oldy)) =>
                                   case smallerthan(getCell brd x y,
                                                    getCell brd oldx oldy) of
                                       true  => (x,y)
                                     | false => (oldx,oldy))
                               minimalSoFar
                               coordinates)
                       (0,0)
                       coordinates
        val (smallx,smally) = coordinateOfSmallestList

        fun findFirstAux [] = NONE
          | findFirstAux (possibleNum::tail)
            = (findFirstSolution(setCell brd smallx smally possibleNum))
               handle NotASolution => findFirstAux tail
    in

        case getCell brd smallx smally of
            [a] => SOME brd
          | lst => findFirstAux lst
    end
