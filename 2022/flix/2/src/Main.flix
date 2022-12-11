def main(): Unit \ Impure =
    discard rockPaperScissors()

def rockPaperScissors(): Result[Unit, String] \ Impure =
    use Result.flatMap;
    let* stringLines = File.readLines("../../2");
    let rounds = stringLines |> List.map(String.splitOn({substr = " "}));
    let scoresPart1 = rounds |> List.map(scorePart1);
    debug(List.foldLeft((a, b) -> a + b, 0, scoresPart1));
    let scoresPart2 = rounds |> List.map(scorePart2);
    debug(List.foldLeft((a, b) -> a + b, 0, scoresPart2));
    Ok(())

def scorePart1(line: List[String]): Int32 =
    match line {
        case left :: right :: Nil => {
            scoreRightPart1(right) + scoreRoundPart1(left, right)
        }
        case _ => unreachable!()
    }

def scoreRightPart1(myPlay: String): Int32 =
    match myPlay {
        case "X" => 1
        case "Y" => 2
        case "Z" => 3
        case _ => unreachable!()
    }

def scoreRoundPart1(theirPlay: String, myPlay: String): Int32 =
    match (theirPlay, myPlay) {
        case ("A", "X") => 3
        case ("B", "Y") => 3
        case ("C", "Z") => 3
        case ("A", "Y") => 6
        case ("B", "Z") => 6
        case ("C", "X") => 6
        case _ => 0
    }

def scorePart2(line: List[String]): Int32 =
    match line {
        case left :: right :: Nil => {
            let rightMove = movePart2(left, right);
            scoreRightPart1(rightMove) + scoreRoundPart1(left, rightMove)
        }
        case _ => unreachable!()
    }

def movePart2(theirPlay: String, outcome: String): String =
    match outcome {
        case "X" => getLose(theirPlay)
        case "Y" => toRound1Notation(theirPlay)
        case "Z" => getWin(theirPlay)
        case _ => unreachable!()
    }

def toRound1Notation(theirPlay: String): String =
    match theirPlay {
        case "A" => "X"
        case "B" => "Y"
        case "C" => "Z"
        case _ => unreachable!()
    }

def getWin(theirPlay: String): String =
    match theirPlay {
        case "A" => "Y"
        case "B" => "Z"
        case "C" => "X"
        case _ => unreachable!()
    }

def getLose(theirPlay: String): String =
    match theirPlay {
        case "A" => "Z"
        case "B" => "X"
        case "C" => "Y"
        case _ => unreachable!()
    }