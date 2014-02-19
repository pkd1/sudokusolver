use "main";
local

    val empty3 = emptyBoard(3)

    val set222 = setCell empty3 2 2 ([2])

in

val test1 = debug(empty3) = Vector.fromList[[1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3],[1, 2, 3], [1, 2, 3], [1, 2, 3]]

val test2 = debug(setCell empty3 0 0 ([2])) = Vector.fromList[[2], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3],[1, 2, 3], [1, 2, 3], [1, 2, 3]]
val test3 = getCell empty3 0 0 = [1, 2, 3]

val test4 = debug(set222) = Vector.fromList[[1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3],[1, 2, 3], [1, 2, 3], [1, 2, 3], [2]]

val test5 = getCell set222 2 2 = [2]

val test6 = (getCell set222 3 2; false)
            handle Subscript => true

val test7 = (getCell set222 2 3; false)
            handle Subscript => true




end;
