package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {

      namedExpressions.transform( (name, signal)  =>  Signal(eval(signal(), namedExpressions, List(name))))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]], used: List[String]): Double = expr match {
    case Literal(v)  => v
    case Plus(a, b) => eval(a, references, used) + eval(b, references, used)
    case Minus(a, b) => eval(a, references, used) - eval(b, references, used)
    case Times(a, b) => eval(a, references, used) * eval(b, references, used)
    case Divide(a, b) => eval(a, references, used) / eval(b, references, used)
    case Ref(name) if used contains name  =>  Double.NaN
    case Ref(name)  =>  eval(getReferenceExpr(name, references), references, name :: used)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
