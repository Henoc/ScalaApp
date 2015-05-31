
import scala.reflect.macros.ParseException
import scala.util.parsing.input.Positional
import scala.util.parsing.combinator._

/**
 * 字句解析フェーズはないので、Tokenがそもそも不要
 *
 * Positional を継承したトークンのパーサーを
 * positioned() で囲めば (token).pos.line, (token).pos.column で位置情報が出る
 *
 * トレイト
 * Stmt > Cluster
 * Stmt(単文) > Expr(式) > Operand > Bindable
 * Cluster = Expr | BlockStmt | ScopeStmt
 * Named = Binder | Operator
 *
 * Clusterはパーサー類の異なる観点からの集合にすぎない、StmtはClusterを包含する
 * Evaluableならeval()で実行可能
 *
 * 新しいASTノードを追加したときに見るべきなのは、eval(), transfer()
 */

sealed trait Cluster extends Stmt
sealed trait Expr extends Stmt with Cluster
sealed trait Operand extends Expr with Positional
sealed trait Bindable extends Operand
sealed trait Named

case class Binder(text : String) extends Operand with Named

case object UnitLiteral extends Bindable
case class NumberLiteral(value : Int) extends Bindable
case class StringLiteral(literal : String) extends  Bindable
case class BooleanLiteral(bool : Boolean) extends Bindable
case object UnderLine extends Bindable
case class Function(params : List[(Binder,Option[Bindable])], var outerEnvOpt : Option[Environment], body : Cluster) extends Bindable {
  /**
   * 無限再帰防止用に、outerEnvを非表示
   * @return
   */
  override def toString() = "Function(" + params.toString() + ", *outerEnv*, " + body.toString + ")"

  /**
   * Bindableが決まっていないパラメータの数を出す
   */
  def restNumOfParams() = {
    params.map{
      case (_,None) => 1
      case _ => 0
    }.sum
  }
}
case class Macro(params : List[Binder], body : Cluster) extends Bindable

case class Operator(opStr : String) extends Expr with Positional with Named{
  val (priority, leftAssoc) = opStr match{
    case "dummy" => (Int.MinValue/100, true)

    case "<-" =>(8,false)
    case "==" =>(9, true)
    case "!=" =>(9, true)
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
case class PrimaryExpr(child : Expr, arguments : List[Cluster]) extends Expr {
  /**
   * アンダーバー以外の引数の数を返す
   */
  def numOfValidArgs() = arguments.map{
    case UnderLine => 0
    case _ => 1
  }.sum
}
case class ClusterExpr(cluster : Cluster) extends Expr // 式の中にクラスターを入れられるようにする構文だまし

sealed trait Stmt
case class ScopeStmt(stmts : List[Stmt]) extends Stmt with Cluster
case class BlockStmt(stmts : List[Stmt]) extends Stmt with Cluster
case object NullStmt extends Stmt
case class IfStmt(condition : Expr, thenBlock : Cluster, elseBlock : Option[Cluster]) extends Stmt
case class WhileStmt(condition : Expr, whileBlock : Cluster) extends Stmt
case class LetStmt(named : Binder, params : Option[List[Binder]], codes : Cluster, typeInfo : Type) extends Stmt
case class MacroStmt(named : Named, params : List[Binder] , codes : Cluster) extends Stmt
case class NativeStmt(operator : String, params : List[Binder]) extends Stmt

/**
 *
 * expr := factorsChain | function
 * factorsChain := factor {op factor}
 * primary := "(" expr ")" | NUMBER | expandable | STRING | BOOLEAN
 * expandable := IDENTIFIER rep(postfix)
 * factor := "-" primary | primary
 * cluster := expr | block | scope
 * statement := ifStatement | whileStatement | letStatement | cluster
 * ifStatement := "if" primary cluster [ "else" cluster ]
 * whileStatement := "while" primary cluster
 * letStatement := typeSeries (";" | "\n") ["macro"] ( letPostfix | letOpPostfix )
 * letPostfix := IDENTIFIER [params] "=" cluster
 * letOpPostfix := op IDENTIFIER IDENTIFIER "=" cluster
 * block := "{" stateLst "}"
 * scope := "[" stateLst "]"
 * stmtLst := [statement] { (";" | "\n") [statement] }
 * oneLine := [statement] (";" | "\n")
 *
 * 関数関連
 * param := IDENTIFIER
 * params := rep1(param)
 * postfix := "(" expr ")" | block | scope | NUMBER | IDENTIFIER | STRING | BOOLEAN | UNDERLINE
 * function := "fun" params "->" cluster
 *
 * 型
 * typeSeries := repsep1( typeInfo , "->" )
 *
 * IDENTIFIER 除外文字列(予約識別子)
 *     fun,if,else,while,_,macro,true,false
 *
 *
 * oneLine が1行分に相当
 * postfix := expr としないことで n - 1 = (n) (-1) = IDENTIFIER postfix になる問題を修正
 *
 * function は expr に追加。primary だと曖昧になるはず
 * fun x -> x * x = function * NUMBER | function になる
 *
 * 現状、 (fun x -> x + 5) 1
 * などの適用の仕方はできない(関数が入った変数への適用のみ)
 */
object BasicParser extends JavaTokenParsers with RegexParsers {

  /**
   * 改行を無視しない
   */
  override val whiteSpace = """( |\t|\x0B|\f|\r)+""".r

  def op : Parser[Operator] =          positioned("""\+|-|\*|\/|<-|==|!=|>|(?!<-)<|%""".r ^^ { case e => Operator(e)})
  def number : Parser[NumberLiteral] = positioned( decimalNumber                 ^^ { case e => NumberLiteral(e.toInt)} )
  def identifier : Parser[Binder] =    positioned("""(?!fun)(?!if)(?!while)(?!else)(?!_)(?!macro)(?!true)(?!false)[a-z]\p{javaJavaIdentifierPart}*""".r ^^ { case e => Binder(e)} )
  def string : Parser[StringLiteral] = positioned( stringLiteral                 ^^ { case e => StringLiteral(e.substring(1,e.length - 1).replace("""\n""","\n"))} )
  def boolean : Parser[BooleanLiteral]=positioned(("true" ^^ {case e => BooleanLiteral(true)} | "false" ^^ {case e => BooleanLiteral(false)}))
  def underline : Parser[Operand] =    positioned("""_""".r ^^ {case e => UnderLine})
  def typeInfo : Parser[AnType]      = positioned("""[A-Z]\p{javaJavaIdentifierPart}*""".r ^^ {case e => AnType(e)})

  def expr : Parser[Expr] = factorsChain | function
  def factorsChain    : Parser[Expr] = factor ~ rep(op ~ factor) ^^ {
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
  
  def primary     : Parser[Expr] =  "(" ~> expr <~ ")" | number | expandable | string | boolean

  /**
   * 変数名または、変数名に引数がついたもの
   * @return 変数名(Binder) or 関数実行節(PrimaryExpr)
   */
  def expandable : Parser[Expr] = identifier ~ rep(postfix) ^^ {
    case id ~ Nil => id
    case id ~ args => PrimaryExpr(id,args)
  }
  def factor      : Parser[Expr] = ("-" ~> primary) ^^ {case p => NegativeExpr(p)} | primary
  
  def cluster : Parser[Cluster] = expr | block | scope
  
  def statement   : Parser[Stmt] = ifStatement | whileStatement | letStatement | cluster
  def ifStatement : Parser[IfStmt] = "if" ~> primary ~ cluster ~ opt("else" ~> cluster) ^^ {
    case cond ~ thenCluster ~ elseClusterOpt => IfStmt(cond,thenCluster,elseClusterOpt)
  }
  def whileStatement : Parser[WhileStmt] = "while" ~> primary ~ cluster ^^ {case cond ~ cls => WhileStmt(cond,cls)}
  def scope : Parser[ScopeStmt] = ("[" ~> stmtLst <~ "]") ^^ {case lst => ScopeStmt(lst.flatten)} // Noneの場合は捨ててリストを構成、BlockStmtのフィールドとする
  def block : Parser[BlockStmt] = ("{" ~> stmtLst <~ "}") ^^ {case lst => BlockStmt(lst.flatten)}
  def stmtLst : Parser[List[Option[Stmt]]] = repsep(opt(statement),";" | "\n")
  def letStatement : Parser[Stmt] = typeSeries ~ ((";" | "\n") ~> opt("macro") ~ (letPostfix | letOpPostfix)) ^^ {
      case tp ~ (None ~    Left(named ~ paramsOpt ~ right))=>        LetStmt (named, paramsOpt, right, tp)
      case tp ~ (Some(_) ~ Left(named ~ paramsOpt ~ right)) =>       MacroStmt(named,paramsOpt.get,right)
      case tp ~ (None ~    Right(named ~ left ~ right ~ content)) => throw new StoneEvalException("演算子定義は未実装")
      case tp ~ (Some(_) ~ Right(named ~ left ~ right ~ content)) => MacroStmt(named,left :: right :: Nil,content)
    }
  def letPostfix = identifier ~ opt(params) ~ ("=" ~> cluster) ^^ (x => Left(x))
  def letOpPostfix = op ~ identifier ~ identifier ~ ("=" ~> cluster) ^^ (x => Right(x))
  def oneLine     : Parser[Stmt] = (opt(statement) ^^ {
    case None => NullStmt
    case Some(s) => s
  } ) <~ (";" | "\n")
  def program : Parser[List[Stmt]] = rep(oneLine)

  //関数関連
  def param = identifier
  def params = rep1(param)
  def postfix = "(" ~> expr <~ ")" | block | scope | number | identifier | string | boolean | underline

  def function : Parser[Function] = "fun" ~> params ~ ("->" ~> cluster) ^^ {
    case lst ~ cls => Function(lst.map(b => (b,None)),None,cls)
  }

  //型
  def typeSeries : Parser[Type] = rep1sep(typeInfo,"->") ^^ {
    case hd :: Nil => hd
    case moreLong => FunctionType(moreLong)
  }


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
