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

  final def testTerm(term: Term): String = showM(interp(term, Seq()))

  private type Environment = Seq[(Name, Value)]

  import Term._
  import Value._

  protected def showval(v: Value): String = v match {
    case Wrong  => "<wrong>"
    case Num(i) => s"$i"
    case Fun(f) => "<function>"
  }

  private def interp(t: Term, e: Environment): M[Value] = t match {
    case Var(name) => lookup(name, e)
    case Con(i)    => unitM(Num(i))
    case Add(a, b) => bindM(interp(a, e)) { a => bindM(interp(b, e)) { b => add(a, b) } }
    case Lam(x, t) => unitM(Fun(xx => interp(t, (x, xx) +: e)))
    case App(f, t) => bindM(interp(f, e)) { f => bindM(interp(t, e)) { t => apply(f, t) } }
  }

  private def lookup(name: Name, e: Environment): M[Value] = e match {
    case Nil            => wrong(s"unbound variable: $name")
    case (n, v) :: rest => if n == name then unitM(v) else lookup(name, rest)
  }

  private def add(a: Value, b: Value): M[Value] = (a, b) match {
    case (Num(a), Num(b)) => unitM(Num(a + b))
    case _                => wrong(s"should be numbers: ${showval(a)}, ${showval(b)}")
  }

  private def apply(f: Value, v: Value): M[Value] = f match {
    case Fun(f) => f(v)
    case _      => wrong(s"should be function: ${showval(f)}")
  }

  protected def wrong(message: String): M[Value] =
    unitM(Wrong)

}

object InterpreterI extends Interpreter:
  type M[A] = A

  override def showM(m: M[Value]): String               = showval(m)
  override def unitM[A](v: A): M[A]                     = v
  override def bindM[A, B](m: M[A])(f: A => M[B]): M[B] = f(m)

object InterpreterE extends Interpreter:
  type M[A] = E[A]

  enum E[A]:
    case Success(a: A)          extends E[A]
    case Error(message: String) extends E[A]

  override def showM(m: M[Value]): String               = showE(m)
  override def unitM[A](v: A): E[A]                     = unitE(v)
  override def bindM[A, B](m: E[A])(f: A => E[B]): E[B] = bindE(m)(f)

  private def showE(m: E[Value]): String = m match {
    case E.Success(a) => s"Success: ${showval(a)}"
    case E.Error(m)   => s"Error: ${m}"
  }

  private def unitE[A](a: A): E[A]       = E.Success(a)
  private def errorE[A](m: String): E[A] = E.Error(m)

  private def bindE[A, B](m: E[A])(f: A => E[B]): E[B] = m match {
    case E.Success(a) => f(a)
    case E.Error(m)   => E.Error(m)
  }

  override def wrong(message: String): E[Value] =
    errorE(message)
