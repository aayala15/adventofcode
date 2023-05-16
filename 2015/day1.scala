@main def main = {
    val source = scala.io.Source.fromFile("day1_input.txt")
    var floor: Int = 0
    var position: Int = 0
    source.getLines().foreach{ line => 
        line.zipWithIndex.foreach { case(x, i) => 
            x match {
                case '(' => floor += 1
                case ')' => floor -= 1
                case _ => 
            }
            if ((floor == -1) && (position == 0)) {
                position = i + 1
            }
        }
    }
    println(s"Santa is in floor: $floor")
    println(s"First time Santa visited the basement: $position")
}