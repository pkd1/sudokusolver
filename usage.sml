
use "solver";

(* read boards from file *)
val boards = readBoardsFile "puzzlefiles/hard9x9.txt"

val firstBoard = hd boards

val solvedBoardOption = findFirstSolution firstBoard;

val boardString = (toString o valOf) solvedBoardOption
    handle Option => "Not a solvable board.";

print boardString
