/**
 * Created by heno on 2015/05/20.
 *
 * Visitor を使って マクロを発見， マクロを置換する
 */

/**
 * 環境の改良版
 * 返り値は評価関数の返り値を使う
 * 環境は可変なので返す必要がない
 * @param nameBind
 * @param outerOpt
 */
case class Field(var nameBind : Map[String,Cluster] = Map.empty, var outerOpt : Option[Field] = None) {
  def mutate(nameBind : Map[String,Cluster] = this.nameBind,
             outerOpt : Option[Field] = this.outerOpt) = {
    this.nameBind = nameBind
    this.outerOpt = outerOpt
    this
  }
}

class Visitor {

  // (AST,環境) -> AST なる恒等関数の環境付属版
  final def envId[T](ast : T, env : Field) = ast

  def inner(env : Field) = Field(outerOpt = Some(env))

  def trStmt(ast:Stmt, env:Field):Stmt = ast match {
      case ss @ ScopeStmt(_) =>     trScopeStmt(ss, inner(env))
      case bs @ BlockStmt(_) =>     trBlockStmt(bs, env)
      case NullStmt => NullStmt
      case is @ IfStmt(_,_,_) =>    trIfStmt(is,env)
      case ws @ WhileStmt(_,_) =>   trWhileStmt(ws,env)
      case ls @ LetStmt(_,_,_) =>   trLetStmt(ls,env)
      case ms @ MacroStmt(_,_,_) => trMacroStmt(ms,env)
      case ns @ NativeStmt(_,_) =>  trNativeStmt(ns,env)
      case expr : Expr =>           trExpr(expr,env)
  }

  def trScopeStmt(ss : ScopeStmt,env : Field):ScopeStmt = ScopeStmt(ss.stmts.map(trStmt(_,env)))
  def trBlockStmt(bs : BlockStmt,env : Field):BlockStmt = BlockStmt(bs.stmts.map(trStmt(_,env)))
  def trIfStmt(is : IfStmt, env : Field):IfStmt =         IfStmt(trExpr(is.condition,env),trCluster(is.thenBlock,env),is.elseBlock.map(trCluster(_,env)))
  def trWhileStmt(ws : WhileStmt, env: Field):WhileStmt = WhileStmt(trExpr(ws.condition,env),trCluster(ws.whileBlock,env))
  def trLetStmt(ls: LetStmt, env :Field):LetStmt =        LetStmt(trBinder(ls.named,env),ls.params.map(_.map(trBinder(_,env))),trCluster(ls.codes,env))
  def trMacroStmt(ms : MacroStmt,env : Field):Stmt =      MacroStmt(trBinder(ms.named,env),ms.params.map(_.map(trBinder(_,env))),trCluster(ms.codes,env))
  def trNativeStmt(ns: NativeStmt,env:Field):NativeStmt = NativeStmt(ns.operator,ns.params.map(trBinder(_,env)))

  def trCluster(cls : Cluster, env : Field) : Cluster = cls match{
    case stmt : Stmt => trStmt(stmt,env).asInstanceOf[Cluster]
    case expr : Expr => trExpr(expr,env).asInstanceOf[Cluster]
  }

  def trExpr(expr : Expr, env : Field) : Expr = expr match{
      case operand : Operand =>     trOperand(operand,env)
      case Operator(opStr) =>       trOperator(Operator(opStr),env)
      case ne @ NegativeExpr(_) =>  trNegativeExpr(ne,env)
      case be @ BinaryExpr(_,_,_) =>trBinaryExpr(be,env)
      case pe @ PrimaryExpr(_,_) => trPrimaryExpr(pe,env)
  }

  def trOperand(operand : Operand, env : Field) : Operand = operand match {
    case bindable : Bindable => trBindable(bindable,env)
    case Binder(text) =>        trBinder(Binder(text),env)
  }

  def trOperator = envId[Operator] _
  def trBinder   = envId[Binder] _

  def trBindable(bindable : Bindable, env : Field) : Bindable = bindable match{
    case UnitLiteral =>            UnitLiteral
    case NumberLiteral(value) =>   trNumberLiteral(NumberLiteral(value),env)
    case StringLiteral(literal) => trStringLiteral(StringLiteral(literal),env)
    case UnderLine => UnderLine
    case f @ Function(_,_,_) => trFunction(f,env)
    case m @ Macro(params,body) => trMacro(m,env)
  }

  def trNumberLiteral = envId[NumberLiteral] _
  def trStringLiteral = envId[StringLiteral] _
  def trFunction      = envId[Function] _
  def trMacro         = envId[Macro] _

  def trNegativeExpr(ne : NegativeExpr, env : Field): NegativeExpr = NegativeExpr(trExpr(ne.primary,env))
  def trBinaryExpr(be : BinaryExpr, env : Field) : BinaryExpr = BinaryExpr(trExpr(be.left,env),trOperator(be.op,env),trExpr(be.right,env))
  def trPrimaryExpr(pe : PrimaryExpr, env : Field) : PrimaryExpr = PrimaryExpr(trExpr(pe.child,env),pe.arguments.map(trCluster(_,env)))
}

object MacroFind extends Visitor {

  override def trFunction = (f,e) => throw new StoneEvalException("マクロ展開中にFunctionに遭遇しました",f)

  override def trMacroStmt(ast : MacroStmt,env : Field) = ast match {
    case MacroStmt(named @ Binder(text),paramsOpt,codes) => paramsOpt match{
      case None => throw new StoneEvalException("引数なしのマクロは定義できません",named)
      case Some(params) => {
        env.mutate(nameBind = env.nameBind + (text -> Macro(params,codes)))
        NullStmt
      }
    }
  }

  override def trExpr(expr : Expr, env : Field) : Expr = expr match{
    case operand : Operand =>     trOperand(operand,env)
    case Operator(opStr) =>       trOperator(Operator(opStr),env)
    case ne @ NegativeExpr(_) =>  trNegativeExpr(ne,env)
    case be @ BinaryExpr(_,_,_) =>trBinaryExpr(be,env)
    case pe @ PrimaryExpr(_,_) => trPrimaryExprAsCluster(pe,env).asInstanceOf[Expr]
  }

  override def trStmt(ast:Stmt, env:Field):Stmt = ast match {
    case prim : PrimaryExpr => trPrimaryExprAsCluster(prim,env).asInstanceOf[Stmt]
    case ss @ ScopeStmt(_) =>     trScopeStmt(ss, inner(env))
    case bs @ BlockStmt(_) =>     trBlockStmt(bs, env)
    case NullStmt => NullStmt
    case is @ IfStmt(_,_,_) =>    trIfStmt(is,env)
    case ws @ WhileStmt(_,_) =>   trWhileStmt(ws,env)
    case ls @ LetStmt(_,_,_) =>   trLetStmt(ls,env)
    case ms @ MacroStmt(_,_,_) => trMacroStmt(ms,env)
    case ns @ NativeStmt(_,_) =>  trNativeStmt(ns,env)
    case expr : Expr =>           trExpr(expr,env)
  }

  /**
   * 返り値が Cluster となる PrimaryExpr
   * マクロ本体も実引数と同様，Exprのときと Stmtのときがあるので，
   * 展開先の文法制約に合わせてキャストさせる必要がある
   * @param prim
   * @param env
   * @return
   */
  def trPrimaryExprAsCluster(prim : PrimaryExpr, env : Field) : Cluster = prim match {
    case PrimaryExpr(binder @ Binder(text), args) => env.nameBind.get(text) match {
      case Some(Macro(params,codes)) => MacroTranslator.transfer(params,prim.arguments,codes)
      case _ => prim
    }
  }

}

/**
 * 環境で指定されたものを見つけたら順次置換する
 */
object MacroTranslator extends Visitor {

  /**
   * Fieldを作成してcodes をtrClusterに投げる
   * @param params
   * @param args
   * @param codes
   * @return
   */
  def transfer(params : List[Binder], args : List[Cluster], codes : Cluster) : Cluster = {
    if(params.length != args.length) throw new StoneEvalException("マクロの仮引数と実引数の数が違います",params.head)
    trCluster(codes,Field(nameBind = Map.empty ++ params.map(_.text).zip(args)))
  }

  override def trFunction = (f,e) => throw new StoneEvalException("マクロ展開中にFunctionに遭遇しました",f)
  override def trExpr(expr : Expr, env : Field) : Expr = expr match{
    case Binder(text) if env.nameBind.contains(text) => env.nameBind(text).asInstanceOf[Expr]
    case operand : Operand =>     trOperand(operand,env)
    case Operator(opStr) =>       trOperator(Operator(opStr),env)
    case ne @ NegativeExpr(_) =>  trNegativeExpr(ne,env)
    case be @ BinaryExpr(_,_,_) =>trBinaryExpr(be,env)
    case pe @ PrimaryExpr(_,_) => trPrimaryExpr(pe,env)
  }

  override def trStmt(ast:Stmt, env:Field):Stmt = ast match {
    case Binder(text) if env.nameBind.contains(text) => env.nameBind(text).asInstanceOf[Stmt]
    case ss @ ScopeStmt(_) =>     trScopeStmt(ss, inner(env))
    case bs @ BlockStmt(_) =>     trBlockStmt(bs, env)
    case NullStmt => NullStmt
    case is @ IfStmt(_,_,_) =>    trIfStmt(is,env)
    case ws @ WhileStmt(_,_) =>   trWhileStmt(ws,env)
    case ls @ LetStmt(_,_,_) =>   trLetStmt(ls,env)
    case ms @ MacroStmt(_,_,_) => trMacroStmt(ms,env)
    case ns @ NativeStmt(_,_) =>  trNativeStmt(ns,env)
    case expr : Expr =>           trExpr(expr,env)
  }
}