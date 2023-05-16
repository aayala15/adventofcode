@main def main = {
    val source = scala.io.Source.fromFile("day7_input.txt")
    var signals = collection.mutable.Map[String, Int]()
    val regexOp = """(?:AND|OR|LSHIFT|RSHIFT|NOT)""".r
    val regexInputs = """\b(?!AND|OR|LSHIFT|RSHIFT|NOT)(\w+)""".r

    source.getLines().foreach { line => 
        var operator = regexOp.findFirstIn(line).getOrElse("ASSIGN")
        var inputs = regexInputs.findAllIn(line).toList
        operator match {
            case "ASSIGN" => { signals(inputs(1)) = inputs(0).toInt }
            case _ =>
        }
    }
    println(signals)
    
}


