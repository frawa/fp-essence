package fpessence

type Name = String

enum Term:
  case Var(name: Name)       extends Term
  case Con(i: Int)           extends Term
  case Add(a: Term, b: Term) extends Term
  case Lam(x: Name, e: Term) extends Term
  case App(f: Term, t: Term) extends Term

trait Interpreter {
  type M[A]

  enum Value:
    case Wrong                     extends Value
    case Num(i: Int)               extends Value
    case Fun(f: Value => M[Value]) extends Value

  protected def showM(m: M[Value]): String
  protected def unitM[A](v: A): M[A]
  protected def bindM[A, B](m: M[A])(f: A => M[B]): M[B]

  def testTerm(term: Term): String = showM(interp(term, Seq()))

  private type Environment = Seq[(Name, Value)]

  protected def showval(v: Value): String = v match {
    case Value.Wrong  => "<wrong>"
    case Value.Num(i) => s"$i"
    case Value.Fun(f) => "<function>"
  }

  private def interp(t: Term, e: Environment): M[Value] = t match {
    case Term.Var(name) => lookup(name, e)
    case Term.Con(i)    => unitM(Value.Num(i))
    case Term.Add(a, b) => bindM(interp(a, e)) { a => bindM(interp(b, e)) { b => unitM(add(a, b)) } }
    case Term.Lam(x, t) => unitM(Value.Fun(xx => interp(t, (x, xx) +: e)))
    case Term.App(f, t) => bindM(interp(f, e)) { f => bindM(interp(t, e)) { t => apply(f, t) } }
  }

  private def lookup(name: Name, e: Environment): M[Value] = e match {
    case Nil            => unitM(Value.Wrong)
    case (n, v) :: rest => if n == name then unitM(v) else lookup(name, rest)
  }

  private def add(a: Value, b: Value): Value = (a, b) match {
    case (Value.Num(a), Value.Num(b)) => Value.Num(a + b)
    case _                            => Value.Wrong
  }

  private def apply(f: Value, v: Value): M[Value] = f match {
    case Value.Fun(f) => f(v)
    case _            => unitM(Value.Wrong)
  }

}

object InterpreterI extends Interpreter:
  type M[A] = A

  protected def showM(m: M[Value]): String               = showval(m)
  protected def unitM[A](v: A): M[A]                     = v
  protected def bindM[A, B](m: M[A])(f: A => M[B]): M[B] = f(m)
