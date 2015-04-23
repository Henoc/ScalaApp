import java.util.Scanner
import scala.util.control.Breaks.{breakable, break}

/**
 * Created by heno on 2015/04/23.
 */
object ParserTest {
  def main(args: Array[String]): Unit = {
    val scan = new Scanner(System.in)
    val str = new StringBuilder
    breakable{ while(scan.hasNextLine){
      val nextLine = scan.nextLine()
      if(nextLine == ":q") break()
      str ++= (nextLine + "\n")
    }}
    println("input: ")
    println(str)
    println()
    println("output: ")
    println(calc(str.toString()))
  }

  def calc(expression : String) = BasicParser.parseAll(BasicParser.program, expression)
}
