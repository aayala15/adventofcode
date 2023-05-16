def initializeGrid(sizeX: Int, sizeY: Int): Map[(Int, Int), Int] = {
    0.to(sizeX).flatMap{ (x: Int) => 
        0.to(sizeY).map{ (y: Int) => 
            (x, y) -> 0
        }
    }.toMap
}


def decodeStringInstruction(instruction: String): (List[Int], List[Int], String) = {
    val regexCord = """(\d+),(\d+)""".r
    val regexAction = """^[a-z]+\s[a-z]*""".r

    
    val coordinates: List[String] = regexCord.findAllIn(instruction).toList
    val start: List[Int] = coordinates(0).split(",").map { x => x.toInt }.toList
    val end: List[Int] = coordinates(1).split(",").map { x => x.toInt }.toList

    val action: String = regexAction.findFirstIn(instruction).getOrElse("turn off").trim
    (start, end, action)
}


def applyInstructionsOnOff(
    grid:  collection.mutable.Map[(Int, Int), Int], 
    start: List[Int],
    end: List[Int],
    action: String
) = {
    start(0).to(end(0)).foreach { x =>
        start(1).to(end(1)).foreach { y =>
            action match {
                case "turn on" => { grid((x, y)) = 1 }
                case "turn off" => { grid((x, y)) = 0 }
                case "toggle" => { grid((x, y)) = 1 - grid((x, y)) }
                case _ =>
            }
        }
    }
}


def applyInstructionsBrightness(
    grid:  collection.mutable.Map[(Int, Int), Int], 
    start: List[Int],
    end: List[Int],
    action: String
) = {
    start(0).to(end(0)).foreach { x =>
        start(1).to(end(1)).foreach { y =>
            action match {
                case "turn on" => { grid((x, y)) += 1 }
                case "turn off" => { grid((x, y)) = (grid((x, y)) - 1).max(0) }
                case "toggle" => { grid((x, y)) += 2 }
                case _ =>
            }
        }
    }
}


@main def main = {
    val source = scala.io.Source.fromFile("day6_input.txt")
    var grid1 = collection.mutable.Map() ++= initializeGrid(1000, 1000)
    var grid2 = collection.mutable.Map() ++= initializeGrid(1000, 1000)

    source.getLines().foreach { line => 
        var (start, end, action) = decodeStringInstruction(line)
        applyInstructionsOnOff(grid1, start, end, action)
        applyInstructionsBrightness(grid2, start, end, action)
    }
    println(s"Total number of lights lit: ${grid1.values.sum}")
    println(s"Total brightness: ${grid2.values.sum}")
}

