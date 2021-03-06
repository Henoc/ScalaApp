import scala.util.parsing.input.Positional

/**
 * Created by heno on 2015/04/24.
 */

/**
 * 環境
 * 関数からの外側環境のアクセスなどで、外側環境が変わっても参照を維持できるように、可変オブジェクトに変更
 * @param nameBind 変数対応表
 * @param outerOpt 外側の環境(スコープ外のグローバル変数への代入などに対応するため)
 * @param ret 返り値
 */
case class Environment (var nameBind : Map[String,Bindable] = Map.empty,
  var outerOpt : Option[Environment] = None,var ret : Bindable = UnitLiteral) {
  /**
   * フィールドを変えて、同じオブジェクトを返す
   * @param nameBind
   * @param outerOpt
   * @param ret
   * @return
   */
  def mutate(nameBind : Map[String,Bindable] = this.nameBind,
             outerOpt : Option[Environment] = this.outerOpt,
              ret : Bindable = this.ret) = {
    this.nameBind = nameBind
    this.outerOpt = outerOpt
    this.ret = ret
    this
  }
}

class StoneEvalException(message : String = null) extends Exception(message){
  def this(mes : String, line : Int, column : Int) = {
    this(mes + " at line: " + line + ", column: " + column)
  }
  def this(mes : String, item : Positional) = {
    this(mes,item.pos.line,item.pos.column)
  }
}

object Evaluator {
  def eval(ast : Stmt, env :Environment) : Environment = ast match {
    case literal : Bindable => env.mutate(ret = literal)
    case leaf : Operand => leaf match {
      case Binder(text) => {
        // 現在の名前空間に無ければ、outer環境も探しに行く
        env.mutate(ret = env.nameBind.get(text) match{
          case Some(bindable) => bindable
          case None => {
            env.outerOpt match {
              case Some(outer) => eval(Binder(text),outer).ret
              case None => throw new StoneEvalException("未知の変数を検出しました",leaf.pos.line,leaf.pos.column)
            }
          }
        })
      }
      case _ => throw new StoneEvalException("???",leaf.pos.line, leaf.pos.column)
    }
    case expr : Expr => expr match {
      case BinaryExpr(left,op,right) => op match{
        case Operator("<-") => left match{
          // 変数に値を束縛し直す
          case Binder(text) => {
            val rightEnv = eval(right,env)
            binder(rightEnv,text,rightEnv.ret)
          }
          case _ => throw new StoneEvalException("代入式の左辺が変数ではありません",op.pos.line, op.pos.column)
        }
        case Operator(opStr) => {
          val leftRet = eval(left,env).ret
          val rightRet = eval(right,env).ret
          val retEnv = (b : Bindable) => env.mutate(ret = b)
          (leftRet,rightRet) match {
            case (NumberLiteral(ln),NumberLiteral(rn)) => opStr match {
              case "+" => retEnv(NumberLiteral(ln + rn))
              case "-" => retEnv(NumberLiteral(ln - rn))
              case "*" => retEnv(NumberLiteral(ln * rn))
              case "/" => retEnv(NumberLiteral(ln / rn))
              case "%" => retEnv(NumberLiteral(ln % rn))
              case "<" => retEnv(BooleanLiteral(ln < rn))
              case ">" => retEnv(BooleanLiteral(ln > rn))
              case "=="=> retEnv(BooleanLiteral(ln == rn))
              case "!="=> retEnv(BooleanLiteral(ln != rn))
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
          case NumberLiteral(n) => env.mutate(ret = NumberLiteral(- n))
          case _ => throw new StoneEvalException("無効な計算です",retEnv.ret.pos.line, retEnv.ret.pos.column)
        }
      }
      // 引数が存在するときのみ生成されている節としいる(多分いいはず)
      case prim @ PrimaryExpr(child,arguments) =>  {
        val retEnv = eval(child,env)
        retEnv.ret match {
          // 関数実行(引数が足りている時)
          case function : Function if prim.numOfValidArgs() == function.restNumOfParams() => {
            // 引数を現在の環境で計算
            // 引数部分で副作用(代入)を起こさないでね
            val evaledArgsIt = arguments.map(eval(_,env).ret).iterator
            val paramNameBinds =
              Map.empty[String,Bindable] ++
                function.params.map{
                  case (Binder(str),None) => (str,evaledArgsIt.next())
                  case (Binder(str),Some(bindable)) => (str,bindable)
                }
            // 関数本体計算用環境 = outer + (callerEnv上で計算した(パラメータ -> 引数)対応表(環境の差分))
            val inner = Environment(paramNameBinds,
              function.outerEnvOpt match{     // outerが入っていなければ現在の環境をouterとする(無名関数用)
                case None => Some(env)
                case other => other
              },
              UnitLiteral)
            // 関数内の、計算実行後環境
            val evaledInner = eval(function.body,inner)
            //// 関数オブジェクトのouterEnvを評価後の外側環境に更新(可変メンバーの参照先を強引に変更)
            ////function.outerEnv = evaledInner.outerOpt.get
            // callerEnvはそのままで、返り値は関数の計算結果
            env.mutate(ret = evaledInner.ret)
          }
          // 引数とパラメータの数が違うとき
          case function : Function if prim.numOfValidArgs() > function.restNumOfParams() => throw new StoneEvalException("関数のパラメータ数に対して、引数の数が過剰です",retEnv.ret.pos.line, retEnv.ret.pos.column)
          // 部分適用
          case function : Function => {
            val evaledArgsIt = arguments.map(eval(_,env).ret).iterator
            // パラメータ対応Bindableが決まってないところについて、まだ引数が残っていたらそれを付ける
            val newFunc = {
              function.copy(params = function.params.map{
                case (binder,None) if evaledArgsIt.hasNext => evaledArgsIt.next() match {
                  case UnderLine => (binder,None)
                  case others => (binder,Some(others))
                }
                case (binder,bindableOpt) => (binder,bindableOpt)
              })
            }
            env.mutate(ret = newFunc)
          }
          case mac : Macro => throw new StoneEvalException("評価時にMacroが存在しています",mac)
        }
      }
      case ClusterExpr(cluster) => eval(cluster,env)
    }
    case stmt : Stmt => stmt match {
      case NullStmt => env
      case IfStmt(condition,thenBlock,elseBlock) => {
        if(eval(condition, env).ret.asInstanceOf[BooleanLiteral].bool) eval(thenBlock, env) else {
          elseBlock match {
            case Some(e) => eval(e,env)
            case None => env.mutate(ret = UnitLiteral)
          }
        }
      }
      case WhileStmt(condition,whileBlock) => {
        var varEnv = env
        while(eval(condition,varEnv).ret.asInstanceOf[BooleanLiteral].bool){
          varEnv = eval(whileBlock,varEnv)
        }
        varEnv
      }
      case ScopeStmt(stmts) => {
        var innerEnv = Environment(Map.empty,Some(env),UnitLiteral)
        for(stmt <- stmts){
          innerEnv = innerEnv.mutate(ret = UnitLiteral)
          innerEnv = eval(stmt,innerEnv)
        }
        // ブロック内で外側環境に与えた変化をもらうため、innerEnvのouterを渡す
        // 返り値はブロック文の最後の文の返り値
        env.mutate(ret = innerEnv.ret)
      }
        // スコープなしの複文
      case BlockStmt(stmts) => {
        var retEnv = env
        for(stmt <- stmts) {
          retEnv = eval(stmt,env)
        }
        retEnv
      }
      case LetStmt(Binder(text),paramsOpt, codes, _) => paramsOpt match {
        case None => {
          val rightEnv = eval(codes,env)
          rightEnv.mutate(nameBind = rightEnv.nameBind + (text -> rightEnv.ret))
        }
        case Some(params) => {
          // 関数の節を作成 定義時の環境(outer)を保存しておく
          val function = Function(params.map(b => (b,None)),Some(env),codes)
          env.mutate(nameBind = env.nameBind + (text -> function), ret = UnitLiteral)
        }
      }

      case MacroStmt(binder @ Binder(text),paramsOpt,codes) => throw new StoneEvalException("評価時にマクロ定義が存在しています",binder)

      case NativeStmt(operator,params) => operator match {
        case "print" => {
          val retEnv = eval(params.head,env)
          print( retEnv.ret match {
            case UnitLiteral => "()"
            case NumberLiteral(n) => n.toString
            case StringLiteral(s) => s
            case Function(_,_,_) => "function"
            case Macro(_,_) => "macro"
            case _ => throw new StoneEvalException("printできないものをprintしようとしました",params.head)
          })
          retEnv.mutate(ret = UnitLiteral)
        }
      }
    }
  }

  /**
   * 環境を再帰的に呼び出してtext変数名にbindableを束縛する
   * 無ければ例外を出す
   * @param env
   * @param text
   * @param bindable
   * @return 更新された環境
   */
  def binder(env:Environment,text:String,bindable:Bindable):Environment = {
    if(env.nameBind.contains(text)) {
      env.mutate(nameBind = env.nameBind.updated(text,bindable))
    }else env.outerOpt match {
      case None => throw new StoneEvalException("未定義の変数には代入できません",bindable.pos.line, bindable.pos.column)
      case Some(outer) => env.mutate(outerOpt = Some(binder(outer,text,bindable)))
    }

  }

  @deprecated
  def boolToNumber(b : Boolean) = if(b) 1 else 0

  @deprecated
  def anyToBool(e : Environment) = e.ret match{
    case NumberLiteral(n) if n == 0 => false
    case _ => true
  }
}
