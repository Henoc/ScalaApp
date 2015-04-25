
/**
 * Created by heno on 2015/04/24.
 */

/**
 * 環境
 * @param nameBind 変数対応表
 * @param ret 返り値
 */
case class Environment (nameBind : Map[String,Bindable] = Map.empty
  ,ret : Bindable = UnitLiteral())

class StoneEvalException(message : String = null) extends Exception(message){
  def this(mes : String, line : Int, column : Int) = {
    this(mes + " at line: " + line + ", column: " + column)
  }
}

object Evaluator {
  def eval(ast : Evaluable, env :Environment) : Environment = ast match {
    case literal : Bindable => Environment(env.nameBind, literal)
    case leaf : Operand => leaf match {
      case Binder(text) => Environment(env.nameBind,
        env.nameBind.getOrElse(text,throw new StoneEvalException("未知の変数を検出しました",leaf.pos.line,leaf.pos.column)))
      case _ => throw new StoneEvalException("???",leaf.pos.line, leaf.pos.column)
    }
    case expr : Expr => expr match {
      case BinaryExpr(left,op,right) => op match{
        case Operator("<-") => left match{
          // 変数に値を束縛し直す
          case Binder(text) if env.nameBind.contains(text) => {
            val rightEnv = eval(right,env)
            val nMap = rightEnv.nameBind.updated(text,rightEnv.ret)
            Environment(nMap,rightEnv.ret)
          }
          case named @ Binder(text) => throw new StoneEvalException("未定義の変数には代入できません",named.pos.line, named.pos.column)
          case _ => throw new StoneEvalException("代入式の左辺が変数ではありません",op.pos.line, op.pos.column)
        }
        case Operator(opStr) => {
          val leftEnv = eval(left,env)
          val rightEnv = eval(right,leftEnv)
          val retEnv = Environment(rightEnv.nameBind,_ : Bindable)
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
      // 引数が存在するときのみ生成されている節としいる(多分いいはず)
      case PrimaryExpr(child,arguments) =>  {
        val retEnv = eval(child,env)
        retEnv.ret match {
          // 関数実行(引数が1つ以上あるとき)
          case function : Function if arguments.length == function.params.length => {
            // 引数を現在の環境で計算
            // 引数部分で副作用(代入)を起こさないでね
            val paramArgsPairLst = for( (param,arg) <- function.params.zip(arguments) ) yield (param.text -> eval(arg, env).ret)
            // 関数本体計算用環境 = outer + (callerEnv上で計算した(パラメータ -> 引数)対応表(環境の差分))
            val newEnv = Environment(function.outerEnv.nameBind ++ paramArgsPairLst, UnitLiteral())
            // 関数内の、計算実行後環境
            val evaledEnv = eval(function.body,newEnv)
            // callerEnvはそのままで、返り値は関数の計算結果
            Environment(env.nameBind,evaledEnv.ret)
          }
          // 引数とパラメータの数が違うとき TODO: 部分適用
          case function : Function => throw new StoneEvalException("関数のパラメータ数と引数の数が一致しません",retEnv.ret.pos.line, retEnv.ret.pos.column)
          case _ => throw new StoneEvalException("関数でないものに引数が与えられています",retEnv.ret.pos.line, retEnv.ret.pos.column)
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
      case LetStmt(Binder(text),paramsOpt, codes) => paramsOpt match {
        case None => {
          val rightEnv = eval(codes,env)
          Environment(rightEnv.nameBind + (text -> rightEnv.ret), rightEnv.ret)
        }
        case Some(params) => {
          // 関数の節を作成 定義時の環境(outer)を保存しておく
          val function = Function(params,env,codes)
          Environment(env.nameBind + (text -> function), UnitLiteral())
        }
      }
    }
  }

  def boolToNumber(b : Boolean) = if(b) 1 else 0

  def anyToBool(e : Environment) = e.ret match{
    case NumberLiteral(n) if n == 0 => false
    case _ => true
  }
}
