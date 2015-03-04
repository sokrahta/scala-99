abstract class Expression {
  def apply: Float
}

class EqualityExpression(a: Expression, b: Expression) {
  import EqualityExpression.floatEqual
  def apply: Boolean = floatEqual(a.apply, b.apply, 0.001F)
}

case class SingletonExpression(a: Float) extends Expression {
  def apply = a
}

case class UnaryExpression(a: Expression, op: String) extends Expression {
  def apply = op match {
    case "+" => a.apply
    case "-" => a.apply
    case _ => throw new NotImplementedError(s"unary operator $op not implemented")
  }
}

case class BinaryExpression(a: Expression, b: Expression, op: String) extends Expression {
  def apply = op match {
    case "*" => a.apply * b.apply
    case "/" => a.apply / b.apply
    case "%" => a.apply % b.apply
    case "+" => a.apply + b.apply
    case "-" => a.apply - b.apply
    case _ => throw new NotImplementedError(s"binary operator $op not implemented")
  }
}

case class TernaryExpression(a: Float, b: Float, c: Float, op: String) extends Expression {
  def apply = ???
}

object Expression {
  def parse(s: String): Expression = {
    // ORDER OF OPERATIONS MUTHA FUCKA
    val paRegex = """^\((.)*\)(^\(\))*$""".r
    val unRegex = """^(-?\d*)$""".r
    val biRegex = """^$""".r
    s match {
      case unRegex(a) => new SingletonExpression(a.toFloat)
      case biRegex(a, op, b) => ???
      case paRegex(a, op, b) => ???
      case _ => ???
    }
  }
}

object EqualityExpression {
  def parse(s: String): EqualityExpression = {
    val t = s.split("=")
    if (t.length != 2) {
      throw new Exception("looking for just one equals (=) sign please")
    } else {
      new EqualityExpression(Expression.parse(t(0)),Expression.parse(t(1)))
    }
  }

  def floatEqual(a: Float, b: Float, epsilon: Float): Boolean = {
    val FLOAT_MIN_NORMAL = 1.17549e-38F
    val (absA, absB, diff) = (Math.abs(a), Math.abs(b), Math.abs(a - b))
    
    if (a == b) {
      true
    } else if (a == 0F || b == 0F || diff < FLOAT_MIN_NORMAL) {
      diff < (epsilon * FLOAT_MIN_NORMAL)
    } else {
      diff / (absA + absB) < epsilon
    }
  }
}

def arithmeticIsValid(exp: String): Boolean = {
  EqualityExpression.parse(exp).apply
}

def arithmetic(is: List[Int]): List[EqualityExpression] = {
  Nil
}

val arith0 = arithmeticIsValid("2-3+5+7 = 11")
val arith1 = arithmeticIsValid("2 = (3*5+7)/11")
val answers = arithmetic(List(2,3,5,7,11))
//2-3+5+7 = 11 or 2 = (3*5+7)/11 (and ten others!).
