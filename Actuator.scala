package actuator



import scala.util._


trait SExp
trait Value extends SExp
case class Error(msg: String, e: SExp) extends Value
case class Number(i: Int) extends Value
case class Var(s: String) extends SExp
case class Symbol(s: String) extends Value
case class Lambda(x: Var, e: SExp) extends Value
case class Op(op: String) extends SExp
case class L(op: SExp, x: SExp*) extends SExp




class Actuator {

  type Bind = (String, Value)

  def env0: List [Bind] = Nil
  def env_+ (s: String, v: Value, env: List [Bind]): List [Bind] =
    (s, v) :: env

  private def lookup (name: String, env: List [Bind]): Option [Value] =
    env match {
      case (x :: xs)  => if (x._1 == name) Some (x._2) else lookup (name, xs)
      case _ => None
    }

  case class Closure (x: Var, e: SExp, env: List [Bind]) extends Value


  def run (e: SExp, env: List [Bind]): Value = e match {

    case Number(i) => Number(i)
    case Symbol(s) => Symbol(s)
    case Closure(v, e, env) => Closure(v, e, env)
    case Lambda(x, exp) => new Closure (x, exp, env)
    case Var(s) => lookup (s, env) match {
      case Some (x) => x
      case None  => Error("No such variable!", Symbol(s))
    }

    /**
      * Bindings
      */
    case L( L(Op (":="), L(Var (s), e)),
            exp) => run (exp, env_+ (s, run (e, env), env))

    /**
      *  Conditions
      */
    case L(Op("if"), L(e, L(e0, e1))) => run(e, env) match {

      case Symbol("true") => run(e0, env)
      case Symbol("false") => run(e1, env)
      case _ => Error("Wrong if function!", L(Op("if"), L(e, L(e0, e1))))
    }

    /**
      *  Support for church numeral
      */
    case L (Number(n), L(op, v)) => {

      var res: Value = run(v, env)
      for (i <- 1 to n) {
        res =  run(L(op, res), env)
      }
      res
    }

    /**
      *  General
      */
    case L (op, v) => {

      (op, v) match { // Basic operators
        case (Op("+"), L (x, y)) => (run (x, env), run (y, env)) match {
          case (Number (m), Number (n)) => Number (m + n)
          case _ => Error("", L(Symbol("+?"), L(x, y)))
        }
        case (Op("-"), L (x, y)) => (run (x, env), run (y, env)) match {
          case (Number (m), Number (n)) => Number (m - n)
          case _ => Error("", L(Symbol("-?"), L(x, y)))
        }
        case (Op("*"), L (x, y)) => (run (x, env), run (y, env)) match {
          case (Number (m), Number (n)) => Number (m * n)
          case _ => Error("", L(Symbol("*?"), L(x, y)))
        }
        case (Op("/"), L (x, y)) => (run (x, env), run (y, env)) match {
          case (Number (m), Number (n)) => Number (m + n)
          case _ => Error("", L(Symbol("/?"), L(x, y)))
        }
        case (Op("="), L (x, y)) => (run (x, env), run (y, env)) match {
          case (Number (m), Number (n)) => if (m == n) Symbol("true") else Symbol("false")
          case _ => Error("", L(Symbol("=?"), L(x, y)))
        }
        case (Op("'"), Var(s)) => Symbol(s)

        case _ => { // lambdas
          val v1 = run (op, env)
          val v2 = run (v, env)
          v1 match {
            case Closure (x, exp, env) => run(L( L(Op(":="), L(x, v2)), exp), env)
            case _ => Error("No operator!", v1)
          }
        }
      }// end (op, v) match
    }
    case unknown => Error("What's this?", unknown)

  } // end run



} // end class Actuator



