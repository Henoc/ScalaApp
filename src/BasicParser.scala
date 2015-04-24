
import scala.util.parsing.input.Positional
import scala.util.parsing.combinator._

/**
 * 字句解析フェーズはないので、Tokenがそもそも不要
 *
 * Positional を継承したトークンのパーサーを
 * positioned() で囲めば (token).pos.line, (token).pos.column で位置情報が出る
 *
 * トレイト
 * Stmt(単文) > Expr(式) > Leaf(オペランド) > Literal
 */

sealed trait Expr extends Stmt
sealed trait Leaf extends Expr with Positional
sealed trait Literal extends Leaf
case class UnitLiteral() extends Literal
case class NumberLiteral(value : Int) extends Literal
case class Name(text : String) extends Leaf
case class StringLiteral(literal : String) extends  Literal
case class Operator(opStr : String) extends Expr with Positional{
  val (priority, leftAssoc) = opStr match{
    case "dummy" => (Int.MinValue/100, true)

    case "<-" => (8,false)
    case "==" =>(9, true)
    case ">" => (9, true)
    case "<" => (9, true)
    case "+" => (10,true)
    case "-" => (10,true)
    case "*" => (11,true)
    case "/" => (11,true)
    case "%" => (11,true)
  }
}

case class NegativeExpr(primary : Expr) extends Expr
case class BinaryExpr(left : Expr , op : Operator, right : Expr) extends Expr

sealed trait Stmt
case class BlockStmt(stmts : List[Stmt]) extends Stmt
case class NullStmt() extends Stmt
case class IfStmt(condition : Expr, thenBlock : BlockStmt, elseBlock : Option[BlockStmt]) extends Stmt
case class WhileStmt(condition : Expr, whileBlock : BlockStmt) extends Stmt
case class LetStmt(named : Name, expr : Expr) extends Stmt

/**
 *
 * expr := factor { op factor }
 * primary := "(" expr ")" | number | identifier | string
 * factor := "-" primary | primary
 * statement := ifStatement | whileStatement | letStatement | simple
 * ifStatement := "if" expr block [ "else block ]
 * whileStatement := "while" expr block
 * letStatement := "let" identifier "=" expr
 * block := "{" [statement] { (";" | "\n") [statement] } "}"
 * simple := expr
 * oneLine := [statement] (";" | "\n")
 *
 * oneLine が1行分に相当
 *
 * TODO: 代入文の導入
 */
object BasicParser extends JavaTokenParsers with RegexParsers {

  /**
   * 改行を無視しない
   */
  override val whiteSpace = """( |\t|\x0B|\f|\r)+""".r

  def op : Parser[Operator] =          positioned("""\+|-|\*|\/|<-|==|>|<|%""".r ^^ { case e => Operator(e)})
  def number : Parser[NumberLiteral] = positioned( decimalNumber                 ^^ { case e => NumberLiteral(e.toInt)} )
  def identifier : Parser[Name] =      positioned( ident                         ^^ { case e => Name(e)} )
  def string : Parser[StringLiteral] = positioned( stringLiteral                 ^^ { case e => StringLiteral(e)} )

  def expr        : Parser[Expr] = factor ~ rep(op ~ factor) ^^ {
    case a ~ lst => {
      // 解析結果の型が扱いづらいので配列(Array[(Operator,Expr)])に直す
      val ary = {
        val ret = new Array[(Operator,Expr)](lst.length + 1)
        var i = 0
        ret(i) = (Operator("dummy"),a) ; i = i + 1
        for((op ~ fc) <- lst) {
          ret(i) = (op,fc)
          i = i + 1
        }
        ret
      }
      makeBinaryExpr(ary, 0 until ary.length)
    }
  }

  def primary     : Parser[Expr] = "(" ~> expr <~ ")" | number | identifier | string
  def factor      : Parser[Expr] = ("-" ~> primary) ^^ {case p => NegativeExpr(p)} | primary
  def statement   : Parser[Stmt] = ifStatement | whileStatement | letStatement | simple
  def ifStatement : Parser[IfStmt] = "if" ~> expr ~ block ~ opt("else" ~> block) ^^ {
    case cond ~ thenBlk ~ elseBlkOpt => IfStmt(cond,thenBlk,elseBlkOpt)
  }
  def whileStatement : Parser[WhileStmt] = "while" ~> expr ~ block ^^ {case cond ~ blk => WhileStmt(cond,blk)}
  def block       : Parser[BlockStmt] = ("{" ~> repsep(opt(statement),";" | "\n") <~ "}") ^^
    {case lst => BlockStmt(lst.flatten)} // Noneの場合は捨ててリストを構成、BlockStmtのフィールドとする
  def letStatement : Parser[LetStmt] = "let" ~> identifier ~ ("=" ~> expr) ^^ {
      case named ~ rightExpr => LetStmt(named, rightExpr)
    }
  def simple      : Parser[Expr] = expr
  def oneLine     : Parser[Stmt] = (opt(statement) ^^ {
    case None => NullStmt()
    case Some(s) => s
  } ) <~ (";" | "\n")
  def program : Parser[List[Stmt]] = rep(oneLine)


  /**
   * 演算子優先度の低いものから節としてまとめる
   * 左結合性、同列順位のものは右のものを先に節にする
   * @param ary (dummyOp, expr1) :: (op1, expr2) :: (op2, expr3) ::  ... :: Nil
   */
  def makeBinaryExpr(ary : Array[(Operator,Expr)], range:Range) : Expr = {
    assert(range.step == 1)
    assert(range.size >= 1)

    if(range.size == 1){
      ary(range.head)._2
    }else {
      var minPr = Int.MaxValue / 100
      var minPrIndex = -1
      for (i <- range.head + 1 to range.last) {
        val op = ary(i)._1
        // 左結合性の演算子は、右にあるものを先に節にする
        if (minPr > op.priority  || (minPr == op.priority && op.leftAssoc)) {
          minPr = op.priority
          minPrIndex = i
        }
      }
      BinaryExpr(
        makeBinaryExpr(ary, range.head until minPrIndex),
        ary(minPrIndex)._1,
        makeBinaryExpr(ary, minPrIndex to range.last)
      )
    }
  }

}
