import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.universe._

trait CaseClass[T] { val defaults: Map[String, Any] }

object CaseClass {
  // see http://stackoverflow.com/questions/14034142/how-do-i-access-default-parameter-values-via-scala-reflection
  // and http://stackoverflow.com/questions/13812172/how-can-i-create-an-instance-of-a-case-class-with-constructor-arguments-with-no
  def instanceNoMacro[T: TypeTag](args: Map[String, Any]): T = {
    val typeT = typeOf[T]
    val classSym = typeT.typeSymbol.asClass
    require(classSym.isCaseClass, s"$typeT must be a case class")
    val mirror = runtimeMirror(getClass.getClassLoader)
    val companionSym = classSym.companion.asModule
    val companionType = companionSym.typeSignature
    val instanceMir = mirror.reflect(mirror.reflectModule(companionSym).instance)
    val ctorSym = typeT.decl(termNames.CONSTRUCTOR).asMethod
    val params = ctorSym.paramLists.head.map(_.name.toString)
    val defaults = params.zipWithIndex.flatMap {
      case (p, i) =>
        val defarg = companionType.member(TermName(s"apply$$default$$${i + 1}"))
        if (defarg == NoSymbol) None
        else Some(p -> instanceMir.reflectMethod(defarg.asMethod)())
    }.toMap
    val classMir = mirror.reflectClass(classSym)
    val ctorMir = classMir.reflectConstructor(ctorSym)
    ctorMir(params.map(p => args.getOrElse(p, defaults(p))): _*).asInstanceOf[T]
  }

  // see http://docs.scala-lang.org/overviews/reflection/overview.html
  def instance[T: TypeTag](args: Map[String, Any])(implicit c: CaseClass[T]): T = {
    val typeT = typeOf[T]
    val classSym = typeT.typeSymbol.asClass
    val mirror = runtimeMirror(getClass.getClassLoader)
    val classMir = mirror.reflectClass(classSym)
    val ctorSym = typeT.decl(termNames.CONSTRUCTOR).asMethod
    val params = ctorSym.paramLists.head.map(_.name.toString)
    val ctorMir = classMir.reflectConstructor(ctorSym)
    ctorMir(params.map(p => args.getOrElse(p, c.defaults(p))): _*).asInstanceOf[T]
  }

  // see http://stackoverflow.com/questions/21958970/scala-macro-get-param-default-value
  def impl[T](c: Context)(T: c.WeakTypeTag[T]): c.Expr[CaseClass[T]] = {
    import c.universe._
    val classSym = T.tpe.typeSymbol.asClass
    require(classSym.isCaseClass, s"${T.tpe} must be a case class")
    val companionSym = classSym.companion
    val applySym = companionSym.typeSignature.decl(TermName("apply")).asMethod
    // can handle only default parameters from the first parameter list
    // because subsequent parameter lists might depend on previous parameters
    val kvps = applySym.paramLists.head.map(_.asTerm).zipWithIndex.flatMap {
      case (p, i) =>
        if (!p.isParamWithDefault) None
        else {
          val getterName = TermName(s"apply$$default$$${i + 1}")
          Some(q"${p.name.toString} -> $companionSym.$getterName")
        }
    }
    c.Expr[CaseClass[T]](q"new CaseClass[$T] { val defaults = Map[String, Any](..$kvps) }")
  }

  // see http://docs.scala-lang.org/overviews/macros/implicits.html
  implicit def materializeCaseClass[T]: CaseClass[T] = macro impl[T]
}
