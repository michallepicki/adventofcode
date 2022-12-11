def main(): Unit \ Impure =
    discard rockPaperScissors()

def rockPaperScissors(): Result[Unit, String] \ Impure =
    use Result.flatMap;
    let* stringLines = File.readLines("../../2");
    let rounds = stringLines |> List.map(String.splitOn({substr = " "}));
    let scoresPart1 = rounds |> List.map(scorePart1);
    debug(List.foldLeft((a, b) -> a + b, 0, scoresPart1));

    // let grouped = splitListBy(stringLines, "");
    // let ints = grouped |> List.map(List.map(stringToInt));
    // let result =
    //     ints
    //     |> List.map(List.foldLeft((a, b) -> a + b, 0))
    //     |> List.sort
    //     |> List.reverse
    //     |> List.take(3);
    // debug(List.take(1, result));
    // debug(List.foldLeft((a, b) -> a + b, 0, result));
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