import scala.collection.mutable.{Map, ListBuffer}

@main def main = {
    val source = scala.io.Source.fromFile("day3_input.txt")
    var log: Map[(Int, Int), Int] = Map((0, 0) -> 1)

    var xCoord: ListBuffer[Int] = ListBuffer(0, 0)
    var yCoord: ListBuffer[Int] = ListBuffer(0, 0)
    source.getLines().foreach{ line =>
        line.zipWithIndex.foreach { case (x, i) =>
            var ii: Int = i % 2
            x match {
                case '^' => { yCoord(ii) += 1 }
                case 'v' => { yCoord(ii) -= 1 }
                case '>' => { xCoord(ii) += 1 }
                case '<' => { xCoord(ii) -= 1 }
                case _ =>
            }
            var balance: Int = log.getOrElse((xCoord(ii), yCoord(ii)), 0)
            log((xCoord(ii), yCoord(ii))) = balance + 1
        }
    }
    println(s"Number of houses receive at least one present: ${log.size}")
}