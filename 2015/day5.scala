import scala.collection.mutable.{Map, ListBuffer}

@main def main(args: String) = {
    val source = scala.io.Source.fromFile("day5_input.txt")
    var numberNiceWords: Int = 0

    source.getLines().foreach{ line =>
        if (args == "first") {
            val regex = """(\w)\1{1,}""".r
            val repeatedLetter: Boolean = regex.pattern.matcher(line).find
            val threeVowels: Boolean = line.filter(Set('a', 'e', 'i', 'o', 'u')).size > 2
            val noSubStrings: Boolean = !(line.matches(".*ab.*|.*cd.*|.*pq.*|.*xy.*"))

            if (threeVowels && noSubStrings && repeatedLetter) numberNiceWords += 1
        } else {
            val regex1 = """.*(\w{2}).*\1{1,}""".r
            val repeatedLetter: Boolean = regex1.pattern.matcher(line).find
            val repeatedSkip: Boolean = 2.to(line.size - 1).map{ i => 
                if (line(i-2) == line(i)) { true }
                else { false }
            }.toList.max

            if (repeatedSkip && repeatedLetter) numberNiceWords += 1
        }

    }
    println(s"Number of nice words: ${numberNiceWords}")
}