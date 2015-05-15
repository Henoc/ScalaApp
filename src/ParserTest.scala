import java.util.Scanner
import scala.util.control.Breaks.{breakable, break}

/**
 * Created by heno on 2015/04/23.
 *
 * 例 フィボナッチ数列
 *
let fib n = {
  if n == 0 {
    0
  } else {
    if n == 1 {
      1
    } else {
      (fib (n - 1)) + (fib (n - 2))
    }
  }
}
fib 10

 例 unless構文マクロ

 let macro unless cond then = {
   if (cond == 0) then
 }

 
 *
 * ブロック文の仕様(最後の式以外の式の返り値はすべてUnitを入れた上で無視する)、
 * if文の仕様(else文省略時に不成立ならUnitが返る)に注意
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
          case _ => {
            println("invalid operator")
          }
        }
      }
    }
  }

  def parse(expression : String) = BasicParser.parseAll(BasicParser.program, expression)
}
