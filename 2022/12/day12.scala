import scala.collection.mutable._

enum Direction (deltaX: Int, deltaY: Int):
    case UP extends Direction(-1, 0)
    case DOWN extends Direction(1, 0)
    case LEFT extends Direction(0, -1)
    case RIGHT extends Direction(0, 1)

    def getDeltaX: Int = { deltaX }
    def getDeltaY: Int = { deltaY }

class Position(x: Int, y: Int) {
    def getX: Int = { x }
    def getY: Int = { y }

    def move(direction: Direction): Position = {
        Position(x + direction.getDeltaX, y + direction.getDeltaY)
    }

    def inRanges(rangeX: Range, rangeY: Range): Boolean = {
        rangeX.contains(x) && rangeY.contains(y)
    }

    def canEqual(a: Any) = a.isInstanceOf[Position]
    override def equals(that: Any): Boolean =
        that match {
            case that: Position => {
                that.canEqual(this) &&
                this.x == that.getX &&
                this.y == that.getY
            }
            case _ => false
        }
}

def findBestPath(
    grid: List[List[Int]],
    start: Position,
    canMoveToElevationFn: (Int, Int) => Boolean,
    endFn: Position => Boolean
):  Int = {
    val rangeX = grid.indices
    val rangeY = grid(0).indices
    val costGrid = Array.tabulate(grid.size, grid(0).size)((_, _) => Int.MaxValue)

    var queue = Queue[(Position, Int)]()
    queue.enqueue((start, 0))

    while (queue.nonEmpty) {
        val curr = queue.dequeue
        val pos = curr(0)
        val cost = curr(1)
        if (costGrid(pos.getX)(pos.getY) > cost) {
            costGrid(pos.getX)(pos.getY) = cost
            if (endFn(pos)) {
                return cost
            }
            
            Direction.values.map { i => pos.move(i) }
                .filter { i => i.inRanges(rangeX, rangeY) }
                .filter { i => canMoveToElevationFn(grid(pos.getX)(pos.getY), grid(i.getX)(i.getY)) }
                .foreach { i => queue.enqueue((i, cost + 1)) }
        }
    }
    Int.MaxValue
}

@main def solvePuzzle(args: String) = {
    var start = Position(0, 0)
    var end = Position(0, 0)
    val otherStarts = new ListBuffer[Position]

    val source = scala.io.Source.fromFile("board.txt")
    val grid =
        source.getLines().zipWithIndex.map{ case (line, x) => 
            line.toList.zipWithIndex.map { case (c, y) =>
                c match {
                    case 'S' => {
                        start = Position(x, y)
                        0
                    }
                    case 'E' => {
                        end = Position(x, y)
                        'z'.toInt - 'a'.toInt
                    }
                    case 'a' => {
                        otherStarts += Position(x, y)
                        0
                    }
                    case _ => {
                        c.toInt - 'a'.toInt
                    }
                }
            }.toList
        }.toList

    def canMoveToElevationFn(from: Int, to: Int): Boolean = to < from + 2
    def endFn(pos: Position): Boolean = pos == end

    val solutions = new ListBuffer[Int]

    println(s"Starting from x=${start.getX} and y=${start.getY}")
    solutions += findBestPath(grid, start, canMoveToElevationFn, endFn)
    if (args == "all") {
        otherStarts.foreach { case x => 
            println(s"Starting from x=${x.getX} and y=${x.getY}")
            solutions += findBestPath(grid, x, canMoveToElevationFn, endFn)
        }
    }
    println(s"Minimum number of steps = ${solutions.min}")
}
