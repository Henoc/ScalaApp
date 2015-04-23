
import scala.util.parsing.input.Positional
import scala.util.parsing.combinator._

/**
 * 字句解析フェーズはないので、Tokenがそもそも不要
 *
 * Positional を継承したトークンのパーサーを
 * positioned() で囲めば (token).pos.line, (token).pos.column で位置情報が出る
 *
 * トレイト
 * Block(複文) > Stmt(単文) > Expr(式) > Leaf(オペレータとオペランド)
 */

sealed trait Expr extends Stmt
sealed trait Leaf extends Expr with Positional
case class NumberLiteral(value : Int) extends Leaf
case class Name(text : String) extends Leaf
case class StringLiteral(literal : String) extends  Leaf
case class Operator(text : String) extends Leaf

case class NegativeExpr(primary : Expr) extends Expr
case class BinaryExpr(left : Expr , op : Operator, right : Expr) extends Expr

sealed trait Block
sealed trait Stmt extends Block
case class BlockStmt(stmts : List[Stmt]) extends Block
case class NullStmt() extends Stmt
case class IfStmt(condition : Expr, thenBlock : Block, elseBlock : Option[Block]) extends Stmt
case class WhileStmt(condition : Expr, whileBlock : Block) extends Stmt

/**
 *
 * expr := factor { op factor }
 * primary := "(" expr ")" | number | identifier | string
 * factor := "-" primary | primary
 * statement := ifStatement | whileStatement | simple
 * ifStatement := "if" expr block [ "else block ]
 * whileStatement := "while" expr block
 * block := "{" [statement] { (";" | "\n") [statement] } "}"
 * simple := expr
 * oneLine := [statement] (";" | "\n")
 *
 * oneLine が1行分に相当
 */
object BasicParser extends JavaTokenParsers with RegexParsers {

  /**
   * 改行を無視しない
   */
  override val whiteSpace = """( |\t|\x0B|\f|\r)+""".r

  def op : Parser[Operator] =          positioned( """\+|-|\*|\/|=|==|>|<|%""".r ^^ { case e => Operator(e)} )
  def number : Parser[NumberLiteral] = positioned( decimalNumber                 ^^ { case e => NumberLiteral(e.toInt)} )
  def identifier : Parser[Name] =      positioned( ident                         ^^ { case e => Name(e)} )
  def string : Parser[StringLiteral] = positioned( stringLiteral                 ^^ { case e => StringLiteral(e)} )

  def expr        : Parser[Expr] = factor ~ rep(op ~ factor) ^^ {
    case a ~ Nil => a
    case a ~ ((b ~ c) :: rest) => BinaryExpr(a,b,exprLoop(c, rest))
  }
  def primary     : Parser[Expr] = "(" ~> expr <~ ")" | number | identifier | string
  def factor      : Parser[Expr] = ("-" ~> primary) ^^ {case p => NegativeExpr(p)} | primary
  def statement   : Parser[Stmt] = ifStatement | whileStatement | simple
  def ifStatement : Parser[Stmt] = "if" ~> expr ~ block ~ opt("else" ~> block) ^^ {
    case cond ~ thenBlk ~ elseBlkOpt => IfStmt(cond,thenBlk,elseBlkOpt)
  }
  def whileStatement : Parser[Stmt] = "while" ~> expr ~ block ^^ {case cond ~ blk => WhileStmt(cond,blk)}
  def block       : Parser[Block] = ("{" ~> repsep(opt(statement),";" | "\n") <~ "}") ^^
    {case lst => BlockStmt(lst.flatten)} // Noneの場合は捨ててリストを構成、BlockStmtのフィールドとする
  def simple      : Parser[Expr] = expr
  def oneLine     : Parser[Stmt] = (opt(statement) ^^ {
    case None => NullStmt()
    case Some(s) => s
  } ) <~ (";" | "\n")
  def program : Parser[List[Stmt]] = rep(oneLine)


  /**
   * expr 用補助関数
   * 演算子の優先順位がまだない(なんでも右から右結合になる)
   * @return
   */
  def exprLoop(operand1 : Expr, rest : List[BasicParser.~[Operator,Expr]]) : Expr = (operand1, rest) match {
    case (_, Nil) => operand1
    case (_, (operator ~ operand2) :: others) => BinaryExpr(operand1,operator,exprLoop(operand2,others))
  }
}
