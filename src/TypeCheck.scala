import scala.util.parsing.input.Positional

/**
 * Created by heno on 2015/05/28.
 */

class StoneTypeException(val mes : String = null) extends Exception(mes)

sealed trait Type
case class AnType(name : String) extends Positional with Type

/**
 * 関数型
 * @param typeLinks (ex. X :: Y :: Z :: Nil は X -> Y -> Z)
 */
case class FunctionType(typeLinks : List[Type]) extends Type

/**
 * 型環境
 * @param nameBind 変数名 -> 型 を記録
 * @param outerOpt
 */
case class TypeField(var nameBind : Map[String,Type] = Map.empty , var outerOpt : Option[TypeField] = None) {
  def mutate(nameBind : Map[String,Type] = this.nameBind,
             outerOpt : Option[TypeField] = this.outerOpt) = {
    this.nameBind = nameBind
    this.outerOpt = outerOpt
    this
  }
}

object TypeCheck {

  def mkMap(typeLst : List[AnType]) : Map[String,AnType] = Map.empty ++ typeLst.map(t => (t.name -> t))

  final val intTypeStr =     "Int"
  final val stringTypeStr =  "String"
  final val booleanTypeStr = "Boolean"
  final val unitTypeStr =    "Unit"
  // 型名 -> 型 を記録
  val types = {
    val intTypeObj     = AnType(intTypeStr)
    val stringTypeObj  = AnType(stringTypeStr)
    val booleanTypeObj = AnType(booleanTypeStr)
    val unitTypeObj    = AnType(unitTypeStr)
    mkMap(intTypeObj :: stringTypeObj :: booleanTypeObj :: unitTypeObj :: Nil)
  }

  def inner(env : TypeField) = TypeField(outerOpt = Some(env))
  def typeCheck(typeObject : Type, typeName : String) : Type = if (types(typeName) != typeObject) throw new StoneTypeException else typeObject
  def typeCheck(typeObject1 : Type, typeObject2 : Type) : Type = if (typeObject1 != typeObject2) throw new StoneTypeException else typeObject1
  def typeCheck(typeObject : Type , typeNames : List[String]) : Type = if(typeNames.map(types(_) == typeObject).reduce(_ | _)) throw new StoneTypeException else typeObject
  def typeTrue(expression : Boolean) : Unit = if(!expression) throw new StoneTypeException

  def chStmt(ast:Stmt, env:TypeField):Type = ast match {
    case ss @ ScopeStmt(_) =>     chScopeStmt(ss, inner(env))
    case bs @ BlockStmt(_) =>     chBlockStmt(bs, env)
    case NullStmt =>              types(unitTypeStr)
    case is @ IfStmt(_,_,_) =>    chIfStmt(is,env)
    case ws @ WhileStmt(_,_) =>   chWhileStmt(ws,env)
    case ls @ LetStmt(_,_,_,_) =>   chLetStmt(ls,env)
    case ms @ MacroStmt(_,_,_) => chMacroStmt(ms,env)
    case ns @ NativeStmt(_,_) =>  chNativeStmt(ns,env)
    case expr : Expr =>           chExpr(expr,env)
  }

  def chScopeStmt(ss : ScopeStmt,env : TypeField):Type      = ss.stmts.map(chStmt(_,inner(env))).lastOption.getOrElse(types(unitTypeStr))
  def chBlockStmt(bs : BlockStmt,env : TypeField):Type      = bs.stmts.map(chStmt(_,env)).lastOption.getOrElse(types(unitTypeStr))
  def chIfStmt   (is : IfStmt,   env : TypeField):Type      = {
    typeCheck(chExpr(is.condition, env),booleanTypeStr)
    is.elseBlock match{
      case None =>            typeCheck(chCluster(is.thenBlock,env),unitTypeStr)
      case Some(elseBlock) => typeCheck(chCluster(is.thenBlock,env),chCluster(elseBlock,env))
    }
  }
  def chWhileStmt(ws : WhileStmt, env:TypeField):Type = {
    typeCheck(chExpr(ws.condition, env),booleanTypeStr)
    chCluster(ws.whileBlock, env)
  }
  def chLetStmt(ls: LetStmt, env : TypeField):Type = ls.params match{
    case None => {
      val resultType = chCluster(ls.codes,env)
      typeCheck(resultType,ls.typeInfo)
      env.mutate(nameBind = env.nameBind + (ls.named.text -> resultType))
      types(unitTypeStr)
    }
    case Some(params) => {
      val fcType = ls.typeInfo.asInstanceOf[FunctionType]
      typeTrue(fcType.typeLinks.length > params.length)
      val paramTypeMap = Map.empty[String,Type] ++ (for((p,t) <- params.zip(fcType.typeLinks)) yield (p.text -> t))
      val resultType = chCluster(ls.codes,TypeField(nameBind = env.nameBind ++ paramTypeMap,Some(env)))
      val restType = fcType.typeLinks.drop(params.length) match{
        case hd :: Nil => hd
        case moreLong => FunctionType(moreLong)
      }
      typeCheck(resultType,restType)
      env.mutate(nameBind = env.nameBind + (ls.named.text -> fcType))
      types(unitTypeStr)
    }
  }
  def chMacroStmt(ms : MacroStmt,env :TypeField):Type = ???
  def chNativeStmt(ns: NativeStmt,env : TypeField):Type = ns.operator match{
    case "print" => {
      val argType = chBinder(ns.params.head,env)
      typeCheck(argType,intTypeStr :: stringTypeStr :: booleanTypeStr :: unitTypeStr :: Nil)
    }
  }

  def chCluster(cls : Cluster, env :TypeField) : Type = cls match{
    case stmt : Stmt => chStmt(stmt,env)
    case expr : Expr => chExpr(expr,env)
  }

  def chExpr(expr : Expr, env :TypeField) : Type = expr match{
    case operand : Operand =>     chOperand(operand,env)
    case Operator(opStr) =>       chOperator(Operator(opStr),env)
    case ne @ NegativeExpr(_) =>  chNegativeExpr(ne,env)
    case be @ BinaryExpr(_,_,_) =>chBinaryExpr(be,env)
    case pe @ PrimaryExpr(_,_) => chPrimaryExpr(pe,env)
    case ce @ ClusterExpr(_) =>   chClusterExpr(ce,env)
  }

  def chNamed(named : Named , env :TypeField) : Type = named match{
    case binder @ Binder(_) => chBinder(binder,env)
    case op @ Operator(_) => chOperator(op,env)
  }

  def chOperand(operand : Operand, env :TypeField) : Type = operand match {
    case bindable : Bindable => chBindable(bindable,env)
    case Binder(text) =>        chBinder(Binder(text),env)
  }

  def chOperator(op : Operator, env : TypeField) : Type = ???
  def chBinder(binder : Binder, env : TypeField) : Type   = env.nameBind(binder.text)

  def chBindable(bindable : Bindable, env :TypeField) : Type = bindable match{
    case UnitLiteral =>            types(unitTypeStr)
    case NumberLiteral(value) =>   chNumberLiteral(NumberLiteral(value),env)
    case StringLiteral(literal) => chStringLiteral(StringLiteral(literal),env)
    case BooleanLiteral(bool) =>   chBooleanLiteral(BooleanLiteral(bool),env)
    case UnderLine => ???
    case f @ Function(_,_,_) =>    chFunction(f,env)
    case m @ Macro(params,body) => chMacro(m,env)
  }

  def chNumberLiteral(nl : NumberLiteral, env : TypeField) : Type = types(intTypeStr)
  def chStringLiteral(sl : StringLiteral, env : TypeField) : Type = types(stringTypeStr)
  def chBooleanLiteral(sl : BooleanLiteral,env: TypeField) : Type = types(booleanTypeStr)
  def chFunction(fc : Function, env : TypeField) : Type           = ???
  def chMacro(mc : Macro, env : TypeField) : Type                 = ???

  def chNegativeExpr(ne : NegativeExpr, env :TypeField): Type     = typeCheck(chExpr(ne.primary,env),intTypeStr)
  def chBinaryExpr(be : BinaryExpr, env :TypeField) : Type        = {
    val leftType  = chExpr(be.left,env)
    val rightType = chExpr(be.right,env)
    be.op.opStr match{
      case "+" => typeTrue((leftType == types(intTypeStr)) && (rightType == types(intTypeStr)) || ((leftType == types(stringTypeStr)) && (rightType == types(stringTypeStr))))
      case "-" => typeTrue((leftType == types(intTypeStr)) && (rightType == types(intTypeStr)))
      case "*" => typeTrue((leftType == types(intTypeStr)) && (rightType == types(intTypeStr)))
      case "/" => typeTrue((leftType == types(intTypeStr)) && (rightType == types(intTypeStr)))
      case "%" => typeTrue((leftType == types(intTypeStr)) && (rightType == types(intTypeStr)))
      case "<" => typeTrue((leftType == types(intTypeStr)) && (rightType == types(intTypeStr)))
      case ">" => typeTrue((leftType == types(intTypeStr)) && (rightType == types(intTypeStr)))
      case "=="=> typeTrue((leftType == types(intTypeStr)) && (rightType == types(intTypeStr)))
      case "!="=> typeTrue((leftType == types(intTypeStr)) && (rightType == types(intTypeStr)))
    }
    be.op.opStr match{
      case "+" | "-" | "*" | "/" | "%" => leftType
      case "<" | ">" | "=="| "!="      => types(booleanTypeStr)
    }
  }
  def chPrimaryExpr(pe : PrimaryExpr, env :TypeField) : Type = {
    val leftType = chExpr(pe.child,env)
    leftType match{
      case FunctionType(typeLinks) => {
        typeApply(typeLinks,pe.arguments.map(chCluster(_,env)),env) match {
          case hd :: Nil => hd
          case others => FunctionType(others)
        }
      }
      case _ => throw new StoneTypeException("引数つき式の最左要素が関数型ではありません")
    }
  }
  def chClusterExpr(ce : ClusterExpr, env :TypeField) : Type = chCluster(ce.cluster,env)

  /**
   * chPrimaryExprの補助関数
   * 関数型に引数の型列を適用してゆき，残りを返す
   * @param fcType
   * @param args
   * @param env
   * @return
   */
  def typeApply(fcType : List[Type], args : List[Type], env : TypeField) : List[Type] = {
    if (fcType == Nil) typeTrue(false)    // 関数の方の型が無くなってしまった場合エラー
    args match{
      case Nil => fcType
      case hd :: rest => {
        typeCheck(hd,fcType.head)
        typeApply(fcType.tail,rest,env)
      }
    }
  }
}