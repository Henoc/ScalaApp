
/**
 * Created by heno on 2015/04/24.
 */

/**
 * 環境
 * @param nameBind 変数対応表
 * @param ret 返り値
 */
case class Environment (nameBind : Map[String,Literal] = Map.empty
  ,ret : Literal = UnitLiteral())

class StoneEvalException(message : String = null) extends Exception(message){
  def this(mes : String, line : Int, column : Int) = {
    this(mes + " at line: " + line + ", column: " + column)
  }
}

object Evaluator {
  def eval(ast : Stmt, env :Environment) : Environment = ast match {
    case literal : Literal => Environment(env.nameBind, literal)
    case leaf : Leaf => leaf match {
      case Name(text) => Environment(env.nameBind,
        env.nameBind.getOrElse(text,throw new StoneEvalException("未知の変数を検出しました",leaf.pos.line,leaf.pos.column)))
      case _ => throw new StoneEvalException("???",leaf.pos.line, leaf.pos.column)
    }
    case expr : Expr => expr match {
      case BinaryExpr(left,op,right) => op match{
        case Operator("<-") => left match{
          // 変数に値を束縛し直す
          case Name(text) if env.nameBind.contains(text) => {
            val rightEnv = eval(right,env)
            val nMap = rightEnv.nameBind.updated(text,rightEnv.ret)
            Environment(nMap,rightEnv.ret)
          }
          case named @ Name(text) => throw new StoneEvalException("未定義の変数には代入できません",named.pos.line, named.pos.column)
          case _ => throw new StoneEvalException("代入式の左辺が変数ではありません",op.pos.line, op.pos.column)
        }
        case Operator(opStr) => {
          val leftEnv = eval(left,env)
          val rightEnv = eval(right,leftEnv)
          val retEnv = Environment(rightEnv.nameBind,_ : Literal)
          (leftEnv.ret,rightEnv.ret) match {
            case (NumberLiteral(ln),NumberLiteral(rn)) => opStr match {
              case "+" => retEnv(NumberLiteral(ln + rn))
              case "-" => retEnv(NumberLiteral(ln - rn))
              case "*" => retEnv(NumberLiteral(ln * rn))
              case "/" => retEnv(NumberLiteral(ln / rn))
              case "%" => retEnv(NumberLiteral(ln % rn))
              case "<" => retEnv(NumberLiteral(boolToNumber(ln < rn)))
              case ">" => retEnv(NumberLiteral(boolToNumber(ln > rn)))
              case "=="=> retEnv(NumberLiteral(boolToNumber(ln == rn)))
            }
            case (StringLiteral(ln),StringLiteral(rn)) => opStr match {
              case "+" => retEnv(StringLiteral(ln + rn))
              case _   => throw new StoneEvalException("文字列リテラルに無効な演算子が使われました", op.pos.line, op.pos.column)
            }
            case _ => throw new StoneEvalException("無効な計算です",op.pos.line, op.pos.column)
          }
        }
      }
      case NegativeExpr(expr) => {
        val retEnv = eval(expr,env)
        retEnv.ret match {
          case NumberLiteral(n) => Environment(retEnv.nameBind, NumberLiteral(- n))
          case _ => throw new StoneEvalException("無効な計算です",retEnv.ret.pos.line, retEnv.ret.pos.column)
        }
      }
    }
    case stmt : Stmt => stmt match {
      case NullStmt() => env
      case IfStmt(condition,thenBlock,elseBlock) => {
        if(anyToBool(eval(condition, env))) eval(thenBlock, env) else {
          elseBlock match {
            case Some(e) => eval(e,env)
            case None => env
          }
        }
      }
      case WhileStmt(condition,whileBlock) => {
        var varEnv = env
        while(anyToBool(eval(condition,varEnv))){
          varEnv = eval(whileBlock,varEnv)
        }
        varEnv
      }
      case BlockStmt(stmts) => stmts match{
        case Nil => env
        case stmt :: rest => eval(BlockStmt(rest), eval(stmt,env))
      }
    }
  }

  def boolToNumber(b : Boolean) = if(b) 1 else 0

  def anyToBool(e : Environment) = e.ret match{
    case NumberLiteral(n) if n == 0 => false
    case _ => true
  }
}
