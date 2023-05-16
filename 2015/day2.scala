def computeWrappingPaperDimensions(d: Array[Int]): (Int, Int) = {
    var paper: Int = 0
    var ribbon: Int = 1
    var min_side: Int = Int.MaxValue
    var min_perimeter: Int = Int.MaxValue

    d.zipWithIndex.foreach{ case(x1, i) =>
        ribbon *= x1
        d.zipWithIndex.foreach{ case(x2, j) =>
            if (i < j) {
                if (x1 * x2 < min_side) {min_side = x1 * x2}
                if (2 * x1 + 2 * x2 < min_perimeter) {min_perimeter = 2 * x1 + 2 * x2}
                paper += 2 * x1 * x2
            }
        }
    }
    paper += min_side
    ribbon += min_perimeter
    (paper, ribbon)
}

@main def main = {
    val source = scala.io.Source.fromFile("day2_input.txt")
    var dimensions = new Array[Int](3)

    var total_paper: Int = 0
    var total_ribbon: Int = 0

    source.getLines().foreach{ line => 
        dimensions = line.split("x").map { x => x.toInt }
        println(dimensions.toList)
        val (p, r) = computeWrappingPaperDimensions(dimensions)
        total_paper += p
        total_ribbon += r
    }
    println(s"Total square feet of wrapping paper: $total_paper")
    println(s"Total feet of ribbon: $total_ribbon")
}