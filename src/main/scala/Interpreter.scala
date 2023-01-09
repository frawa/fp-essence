package fpessence

import fpessence.Interpreter.E

// TODO opaque
type Name = String

opaque type Position = Int
object Position:
  val pos0: Position          = 0
  def apply(p: Int): Position = p

enum Term:
  case Var(name: Name)          extends Term
  case Con(i: Int)              extends Term
  case Add(a: Term, b: Term)    extends Term
  case Lam(x: Name, e: Term)    extends Term
  case App(f: Term, t: Term)    extends Term
  case At(p: Position, t: Term) extends Term

trait TheMonad[M[_]]:
  def showM(m: M[Value]): String
  def unitM[A](v: A): M[A]
  def bindM[A, B](m: M[A])(f: A => M[B]): M[B]

enum Value:
  case Wrong                               extends Value
  case Num(i: Int)                         extends Value
  case Fun[M[Value]](f: Value => M[Value]) extends Value

class Interpreter[M[_]](using TheMonad[M]):

//   enum Value:
//     case Wrong                     extends Value
//     case Num(i: Int)               extends Value
//     case Fun(f: Value => M[Value]) extends Value

//   protected def showM(m: M[Value]): String
//   protected def unitM[A](v: A): M[A]
//   protected def bindM[A, B](m: M[A])(f: A => M[B]): M[B]
  protected val m = summon[TheMonad[M]]

  final def testTerm(term: Term): String = m.showM(interp(term, Seq()))

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
    case Con(i)    => m.unitM(Num(i))
    case Add(a, b) => m.bindM(interp(a, e)) { a => m.bindM(interp(b, e)) { b => add(a, b) } }
    case Lam(x, t) => m.unitM(Fun(xx => interp(t, (x, xx) +: e)))
    case App(f, t) => m.bindM(interp(f, e)) { f => m.bindM(interp(t, e)) { t => apply(f, t) } }
  }

  private def lookup(name: Name, e: Environment): M[Value] = e match {
    case Nil            => wrong(s"unbound variable: $name")
    case (n, v) :: rest => if n == name then m.unitM(v) else lookup(name, rest)
  }

  private def add(a: Value, b: Value): M[Value] = (a, b) match {
    case (Num(a), Num(b)) => m.unitM(Num(a + b))
    case _                => wrong(s"should be numbers: ${showval(a)}, ${showval(b)}")
  }

  private def apply(f: Value, v: Value): M[Value] = f match {
    case Fun[M](f) => f(v) // ff.f(v)
    case _         => wrong(s"should be function: ${showval(f)}")
  }

  protected def wrong(message: String): M[Value] =
    m.unitM(Wrong)

object Interpreter:
  import Value._

  type I[A] = A

  given TheMonad[I] with
    def showM(m: I[Value]): String               = showval(m)
    def unitM[A](v: A): I[A]                     = v
    def bindM[A, B](m: I[A])(f: A => I[B]): I[B] = f(m)

  def showval(v: Value): String = v match {
    case Wrong  => "<wrong>"
    case Num(i) => s"$i"
    case Fun(f) => "<function>"
  }

  enum E[A]:
    case Success(a: A)          extends E[A]
    case Error(message: String) extends E[A]

  given TheMonad[E] with
    def showM(m: E[Value]): String               = showE(m)
    def unitM[A](v: A): E[A]                     = unitE(v)
    def bindM[A, B](m: E[A])(f: A => E[B]): E[B] = bindE(m)(f)

  private def showE(m: E[Value]): String = m match {
    case E.Success(a) => s"Success: ${showval(a)}"
    case E.Error(m)   => s"Error: ${m}"
  }
  private def unitE[A](a: A): E[A] = E.Success(a)
  def errorE[A](m: String): E[A]   = E.Error(m)
  private def bindE[A, B](m: E[A])(f: A => E[B]): E[B] = m match {
    case E.Success(a) => f(a)
    case E.Error(m)   => E.Error(m)
  }

  type P[A] = Position => E[A]

  given TheMonad[P] with
    def showM(m: P[Value]): String               = showP(m)
    def unitM[A](v: A): P[A]                     = unitP(v)
    def bindM[A, B](m: P[A])(f: A => P[B]): P[B] = bindP(m)(f)

  private def showP(m: P[Value]): String =
    showE(m(Position.pos0))

  private def unitP[A](v: A): P[A]                     = p => unitE(v)
  private def bindP[A, B](m: P[A])(f: A => P[B]): P[B] = ???

class InterpreterE extends Interpreter[Interpreter.E](using Interpreter.given_TheMonad_E):
  override protected def wrong(message: String): E[Value] =
    Interpreter.errorE(message)
