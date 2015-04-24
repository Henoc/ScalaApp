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
    val ast = parse(str.toString())
    println(ast)
    println("evaluate: ")
    println(evalList(ast.get, Environment()))
  }

  def parse(expression : String) = BasicParser.parseAll(BasicParser.program, expression)

  def evalList(ast : List[Stmt] , env : Environment) : Literal = ast match {
    case Nil => env.ret
    case hd :: tl => evalList(tl, Evaluator.eval(hd,env))
  }
}
