import java.util.Scanner
import scala.util.control.Breaks.{breakable, break}

/**
 * Created by heno on 2015/04/23.
 */
object ParserTest {

  val scan = new Scanner(System.in)
  var env = new Environment()
  val str = new StringBuilder

  def main(args: Array[String]) {
    breakable{
      while(scan.hasNext){
        val line = scan.nextLine() + '\n'
        if(line.charAt(0) != ':') str append line
        else line.charAt(1) match{
          case 'e' => {
            try{
              val stmtLst = parse(str.toString()).get
              for(stmt <- stmtLst) env = Evaluator.eval(stmt,env)
              println("parsed: " + stmtLst)
              println("ans: " + env.ret)
            }
            catch{
              case e => println(e)
            }
            str.clear()
          }
          case 'q' => {
            break()
          }
          case 's' => {
            println(env)
          }
        }
      }
    }
  }

  def parse(expression : String) = BasicParser.parseAll(BasicParser.program, expression)
}
