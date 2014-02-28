use "main.sml";

fun test () =
    let
        val empty4 = emptyBoard(4)
        val set222 = setCell empty4 2 2 2
        val all4 = [1,2,3,4]
        val set4 = setCell
                       (setCell
                            (setCell (emptyBoard 4)
                                     0 0 1)
                            1 0 2)
                       2 0 3

        val tests = [
            (1, debug(empty4) =
                Vector.fromList [all4,all4,all4,all4,
                                 all4,all4,all4,all4,
                                 all4,all4,all4,all4,
                                 all4,all4,all4,all4]),

            (2, debug(setCell empty4 0 0 1)
                = Vector.fromList[[1],    [2,3,4],  [2,3,4],   [2,3,4],
                                  [2,3,4],[2,3,4],  [1,2,3,4], [1,2,3,4],
                                  [2,3,4],[1,2,3,4],[1,2,3,4], [1,2,3,4],
                                  [2,3,4],[1,2,3,4],[1,2,3,4], [1,2,3,4]]),

            (3, getCell empty4 0 0 = [1, 2, 3, 4]),

            (4, debug(set222) =
                Vector.fromList[[1,2,3,4],[1,2,3,4],[1,3,4],[1,2,3,4],
                                [1,2,3,4],[1,2,3,4],[1,3,4],[1,2,3,4],
                                [1,  3,4],[1,3,4]  ,[2],    [1,3,4],
                                [1,2,3,4],[1,2,3,4],[1,3,4],[1,3,4]]),

            (5, getCell set222 2 2 = [2]),

            (6, (getCell set222 4 0; false) handle Subscript => true),

            (7, (getCell set222 3 4; false) handle Subscript => true),

            (8, getCell (emptyBoard(9)) 8 8 = [1,2,3,4,5,6,7,8,9]),

            (9, (getCell (emptyBoard(9)) 9 0; false) handle Subscript => true),
            (10, (getCell (emptyBoard(9)) 0 9; false) handle Subscript => true),
            (11, debug (setCell (setCell (setCell (emptyBoard 4)
                                                  0 0 1)
                                         1 0 2)
                                2 2 3)
                 = Vector.fromList[[1],       [2],       [4],    [3],
                            [3, 4],    [3, 4],    [1, 2], [1, 2],
                            [2, 4],    [1, 4],    [3],    [1, 2, 4],
                            [2, 3, 4], [1, 3, 4], [1, 2], [1, 2, 4]]),

            (12, debug (setCell (setCell (setCell (setCell (emptyBoard 4)
                                                           0 0 1)
                                                  1 0 2)
                                         2 2 3)
                                1 3 4)
                 = fromList[[1], [2], [4],    [3],
                            [4], [3], [1, 2], [1, 2],
                            [2], [1], [3],    [4],
                            [3], [4], [1, 2], [1, 2]])

        ]
        val allPassed = List.all (fn (_,b) => b) tests
        val failedTests = List.foldl (fn ((x,b),s) => if not b then s^(Int.toString(x))^", " else s) "" tests
    in
        if allPassed then
            print "\nSUCCESS!\n\n"
        else
            print ("\nFAILED TESTS: " ^ failedTests ^ "\n\n")
    end;
test();
