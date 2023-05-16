import scala.util.control.Breaks._
import java.security.MessageDigest

def getMd5(str: String): String = {
    MessageDigest
        .getInstance("MD5")
        .digest(str.getBytes("UTF-8"))
        .map("%02x".format(_))
        .mkString("")
}

@main def main = {
    val key: String = "ckczppom"
    val leadingZeros: Int = 6
    var number: Int = 0
    var keepLoop: Boolean = true
    
    while (keepLoop) {
        number += 1
        var md5Hash: String = getMd5(key + number.toString)
        if ((number > Int.MaxValue - 1) || 
            (md5Hash.substring(0, leadingZeros) == "0" * leadingZeros)) keepLoop = false
    }
    println(s"Lowest positive number: ${number}")
}