use "main";
local

    val empty3 = emptyBoard(3)

    val set222 = setCell empty3 2 2 ([2])

in

fun id x = x

fun test 0 = true
  | test 1 = debug(empty3) = Vector.fromList[[1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3],[1, 2, 3], [1, 2, 3], [1, 2, 3]]

  | test 2 = debug(setCell empty3 0 0 ([2])) = Vector.fromList[[2], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3],[1, 2, 3], [1, 2, 3], [1, 2, 3]]
 | test 3 = getCell empty3 0 0 = [1, 2, 3]

 | test 4 = debug(set222) = Vector.fromList[[1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3],[1, 2, 3], [1, 2, 3], [ 2, 3], [2]]

 | test 5 = getCell set222 2 2 = [2]

 | test 6 = ((getCell set222 3 2; false) handle Subscript => true)

 | test 7 = ((getCell set222 2 3; false) handle Subscript => true)

 | test _ = raise Domain

val tests = List.tabulate (8, (fn x => (x,test x)))
val allTests = List.all (fn (_,b) => b) tests

end;
