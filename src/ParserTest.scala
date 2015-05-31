import java.util.Scanner
import scala.util.control.Breaks.{breakable, break}

/**
 * Created by heno on 2015/04/23.
 *
 *
 * ブロック文の仕様(最後の式以外の式の返り値はすべてUnitを入れた上で無視する)、
 * if文の仕様(else文省略時に不成立ならUnitが返る)に注意
 */
object ParserTest {

  /**
   * 便利なネイティブ関数など
   */
  val utilities : Map[String,Bindable] = Map.empty[String,Bindable] +
    ("print" -> Function((Binder("#"),None) :: Nil,None,BlockStmt(NativeStmt("print",Binder("#") :: Nil) :: Nil))) +
    ("println" -> Function((Binder("#"),None) :: (Binder("#2"),Some(StringLiteral("\n"))) :: Nil,None,BlockStmt(NativeStmt("print",Binder("#") :: Nil) :: NativeStmt("print",Binder("#2") :: Nil) :: Nil)))

  val scan = new Scanner(System.in)
  var env = new Environment(nameBind = utilities)
  val fld = Field() // マクロ用環境．本当はEnvironmentと統合したい
  val typefld = TypeField()
  val str = new StringBuilder

  def main(args: Array[String]) {
    breakable{
      while(scan.hasNext){
        val line = scan.nextLine()
        if(!line.matches(""":.""")) str append (line + '\n')
        else line.charAt(1) match{
          case 'e' => {
            try{
              val stmtLst = parse(str.toString()).get
              val expandedLst = for(stmt <- stmtLst) yield MacroFind.trStmt(stmt,fld)
              stmtLst.map(TypeCheck.chStmt(_,typefld))
              for(stmt <- expandedLst) env = Evaluator.eval(stmt,env)
              println("parsed: ")
              stmtLst.map(println)
              println("macro expanded: ")
              expandedLst.map(println)
              println("type checked")
              println("ans: " + env.ret)
            }
            catch{
              case e => e.printStackTrace()
            }
            str.clear()
          }
          case 'q' => {
            break()
          }
          case 's' => {
            println(env)
            println(fld)
            println(typefld)
          }
          case 'm' => {
            try{
              val stmtLst = parse(str.toString()).get
              val expandedLst = for(stmt <- stmtLst) yield MacroFind.trStmt(stmt,fld)
              println("parsed: ")
              stmtLst.map(println)
              println("field: " + fld)
              println("macro expanded: ")
              expandedLst.map(println)
            }catch{
              case e => e.printStackTrace()
            }
            str.clear()
          }
          case 't' => {
            try{
              val stmtLst = parse(str.toString()).get
              stmtLst.map(TypeCheck.chStmt(_,typefld))
              println("type checked")
            }catch{
              case e => e.printStackTrace()
            }
            str.clear()
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
