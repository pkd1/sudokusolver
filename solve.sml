
use "solver";

val instr = TextIO.stdIn

fun stringListFromStdIn () = case TextIO.inputLine instr of
                                 NONE => (TextIO.closeIn instr;[])
                               | SOME "\n" => stringListFromStdIn ()
                               | SOME line => line::(stringListFromStdIn ())

val stringList = stringListFromStdIn ()

val board = readBoard stringList
    handle MalformattedBoard => (print "Invalid board.";
                                 OS.Process.exit OS.Process.failure)

val _ = case findFirstSolution board of
            NONE => (print "No solution exists.";
                    OS.Process.exit OS.Process.failure)
          | (SOME b) => printBoard b;

val _ = OS.Process.exit OS.Process.success;
