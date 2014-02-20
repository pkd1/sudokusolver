use "main";

fun test () =
    let
        val empty3 = emptyBoard(3)
        val set222 = setCell empty3 2 2 ([2])
        val tests = [
            (1, debug(empty3) = Vector.fromList[[1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3],[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
            
            (2, debug(setCell empty3 0 0 ([2])) = Vector.fromList[[2], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3],[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
            
            (3, getCell empty3 0 0 = [1, 2, 3]),
            
            (4, debug(set222) = Vector.fromList[[1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3],[1, 2, 3], [1, 2, 3], [1, 2, 3], [2]]),
            
            (5, getCell set222 2 2 = [2]),
            
            (6, (getCell set222 3 2; false) handle Subscript => true),
            
            (7, (getCell set222 2 3; false) handle Subscript => true),
                
            (8, getCell (emptyBoard(9)) 8 8 = [1,2,3,4,5,6,7,8,9]),
            
            (9, (getCell (emptyBoard(9)) 9 0; false) handle Subscript => true),
            (10, (getCell (emptyBoard(9)) 0 9; false) handle Subscript => true)
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
