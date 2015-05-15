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
  var outerOpt : Option[Environment] = None,var ret : Bindable = UnitLiteral()) {
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
  def eval(ast : Evaluable, env :Environment) : Environment = ast match {
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
              case "<" => retEnv(NumberLiteral(boolToNumber(ln < rn)))
              case ">" => retEnv(NumberLiteral(boolToNumber(ln > rn)))
              case "=="=> retEnv(NumberLiteral(boolToNumber(ln == rn)))
              case "!="=> retEnv(NumberLiteral(boolToNumber(ln != rn)))
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
              UnitLiteral())
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
                  case UnderLine() => (binder,None)
                  case others => (binder,Some(others))
                }
                case (binder,bindableOpt) => (binder,bindableOpt)
              })
            }
            env.mutate(ret = newFunc)
          }
          case mac : Macro => {
            if (prim.numOfValidArgs() != mac.params.length) throw new StoneEvalException("マクロの引数の数が違います",mac.pos.line,mac.pos.column)

            eval(transfer(mac,arguments),env)
          }
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
            case None => env.mutate(ret = UnitLiteral())
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
      case ScopeStmt(stmts) => {
        var innerEnv = Environment(Map.empty,Some(env),UnitLiteral())
        for(stmt <- stmts){
          innerEnv = innerEnv.mutate(ret = UnitLiteral())
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
      case LetStmt(Binder(text),paramsOpt, codes) => paramsOpt match {
        case None => {
          val rightEnv = eval(codes,env)
          rightEnv.mutate(nameBind = rightEnv.nameBind + (text -> rightEnv.ret))
        }
        case Some(params) => {
          // 関数の節を作成 定義時の環境(outer)を保存しておく
          val function = Function(params.map(b => (b,None)),Some(env),codes)
          env.mutate(nameBind = env.nameBind + (text -> function), ret = UnitLiteral())
        }
      }

      case MacroStmt(binder @ Binder(text),paramsOpt,codes) => paramsOpt match {
        case None => throw new StoneEvalException("引数なしのマクロは定義できません",binder.pos.line,binder.pos.column)
        case Some(params) => env.mutate(nameBind = env.nameBind + (text -> Macro(params,codes)), ret = UnitLiteral())
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

  /**
   * マクロ用、AST置換関数
   * 部分木のtransferでは、返り値の型が決まっているので、asInstanceOfを使っている
   * ここでエラーが出ることは無いはず・・・
   * @param mac
   * @param args
   * @return
   */
  def transfer(mac : Macro,args : List[Cluster]):Cluster = {
    // Macroのbodyのみを変えて、同じ仮引数、実引数でtransferを実行する
    val changeBodyAndTransfer = (newBody : Cluster) => transfer(mac.copy(body = newBody),args)
    // 複文用、Stmtに対してのパターンマッチ
    val stmtCases = (stmt : Stmt) => stmt match{
      case blk : ScopeStmt => changeBodyAndTransfer(blk).asInstanceOf[ScopeStmt]
      case ns : NullStmt => ns
      case ifStmt : IfStmt => {
        val trCond = changeBodyAndTransfer(ifStmt.condition).asInstanceOf[Expr]
        val trThen = changeBodyAndTransfer(ifStmt.thenBlock)
        val trElseOpt = ifStmt.elseBlock match{
          case None => None
          case Some(elseBlock) => Some(changeBodyAndTransfer(elseBlock))
        }
        IfStmt(trCond,trThen,trElseOpt)
      }
      case ws : WhileStmt => {
        val trCond = changeBodyAndTransfer(ws.condition).asInstanceOf[Expr]
        val trBlock = changeBodyAndTransfer(ws.whileBlock)
        WhileStmt(trCond,trBlock)
      }
      case ls : LetStmt => {
        val trNamed = changeBodyAndTransfer(ls.named).asInstanceOf[Binder]
        val trParams = ls.params match {
          case None => None
          case Some(params) => Some(params.map(changeBodyAndTransfer(_).asInstanceOf[Binder]))
        }
        val trCodes = changeBodyAndTransfer(ls.codes)
        LetStmt(trNamed,trParams,trCodes)
      }
      case ms : MacroStmt => {
        val trNamed = changeBodyAndTransfer(ms.named).asInstanceOf[Binder]
        val trParams = ms.params match {
          case None => None
          case Some(params) => Some(params.map(changeBodyAndTransfer(_).asInstanceOf[Binder]))
        }
        val trCodes = changeBodyAndTransfer(ms.codes)
        MacroStmt(trNamed,trParams,trCodes)
      }
      case expr : Expr => changeBodyAndTransfer(expr).asInstanceOf[Expr]
      case _ => throw new StoneEvalException("マクロ置換中、未知のStmtクラスに遭遇しました",mac)
    }

    // パターンマッチ本体
    mac.body match {
      case expr : Expr => expr match {
        case ne : NegativeExpr => NegativeExpr(changeBodyAndTransfer(ne.primary).asInstanceOf[Expr])
        case pe : PrimaryExpr => {
          val trChild = changeBodyAndTransfer(pe.child).asInstanceOf[Expr]
          val trArgs = pe.arguments.map(e => changeBodyAndTransfer(e))
          PrimaryExpr(trChild,trArgs)
        }
        case be : BinaryExpr => {
          val trLeft = changeBodyAndTransfer(be.left).asInstanceOf[Expr]
          val trRight = changeBodyAndTransfer(be.right).asInstanceOf[Expr]
          BinaryExpr(trLeft,be.op,trRight)
        }
        case oe : Operator => throw new StoneEvalException("マクロ置換中、オペレータを置換しようとしました",oe)
        case operand : Operand => operand match {
          case bindable : Bindable => bindable
          case binder : Binder => {
            (for((param,arg) <- mac.params.zip(args) if param == binder) yield arg) match{
              case Nil => binder
              case arg :: Nil => arg
              case _ => throw new StoneEvalException("複数のマクロパラメータが同一の可能性があります",binder)
            }
          }
          case _ => throw new StoneEvalException("マクロ置換中、未知のOperandクラスに遭遇しました",mac)
        }
        case _ => throw new StoneEvalException("マクロ置換中、未知のExprクラスに遭遇しました",mac)
      }
      case ScopeStmt(stmts) => ScopeStmt(stmts.map(stmtCases(_)))
      case BlockStmt(stmts) => BlockStmt(stmts.map(stmtCases(_)))
    }
  }

  def boolToNumber(b : Boolean) = if(b) 1 else 0

  def anyToBool(e : Environment) = e.ret match{
    case NumberLiteral(n) if n == 0 => false
    case _ => true
  }
}
