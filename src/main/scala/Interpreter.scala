package fpessence

import fpessence.Interpreter.E
import fpessence.Interpreter.P
import fpessence.Interpreter.S

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
  case Count                    extends Term

trait TheMonad[M[_]]:
  def showM(m: M[Value]): String
  def unitM[A](v: A): M[A]
  def bindM[A, B](m: M[A])(f: A => M[B]): M[B]

enum Value:
  case Wrong                               extends Value
  case Num(i: Int)                         extends Value
  case Fun[M[Value]](f: Value => M[Value]) extends Value

class Interpreter[M[_]](using TheMonad[M]):
  final def testTerm(term: Term): String = m.showM(interp(term, Seq()))

  protected val m = summon[TheMonad[M]]

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
    case Add(a, b) => doAdd(interp(a, e), interp(b, e))
    case Lam(x, t) => m.unitM(Fun(xx => interp(t, (x, xx) +: e)))
    case App(f, t) => doApply(interp(f, e), interp(t, e))
    case At(p, t)  => reset(p, interp(t, e))
    case Count     => count()
  }

  protected def doAdd(a: M[Value], b: M[Value]): M[Value] =
    m.bindM(a) { a => m.bindM(b) { b => doAdd(a, b) } }
  protected def doApply(f: M[Value], v: M[Value]): M[Value] =
    m.bindM(f) { f => m.bindM(v) { v => doApply(f, v) } }

  private def lookup(name: Name, e: Environment): M[Value] = e match {
    case Nil            => wrong(s"unbound variable: $name")
    case (n, v) :: rest => if n == name then m.unitM(v) else lookup(name, rest)
  }

  final def doAdd(a: Value, b: Value): M[Value] = (a, b) match {
    case (Num(a), Num(b)) => m.unitM(Num(a + b))
    case _                => wrong(s"should be numbers: ${showval(a)}, ${showval(b)}")
  }

  final def doApply(f: Value, v: Value): M[Value] = f match {
    case Fun[M](f) => f(v) // ff.f(v)
    case _         => wrong(s"should be function: ${showval(f)}")
  }

  protected def wrong(message: String): M[Value] =
    m.unitM(Wrong)

  protected def reset(p: Position, m: M[Value]): M[Value] =
    m

  protected def count(): M[Value] =
    wrong("cannot count")

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

  def showpos(p: Position): String =
    s"[$p]"

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

  private def showP(m: P[Value]): String = showE(m(Position.pos0))
  def errorP[A](m: String): P[A]         = p => errorE(s"${showpos(p)}: $m")

  private def unitP[A](v: A): P[A]                     = p => unitE(v)
  private def bindP[A, B](m: P[A])(f: A => P[B]): P[B] = p => bindE(m(p))(x => f(x)(p))

  def resetP[A](p: Position, m: P[A]): P[A] = q => m(p)

  type State = Int
  type S[A]  = State => (A, State)

  given TheMonad[S] with
    def showM(m: S[Value]): String               = showS(m)
    def unitM[A](v: A): S[A]                     = unitS(v)
    def bindM[A, B](m: S[A])(f: A => S[B]): S[B] = bindS(m)(f)

  private def showS(m: S[Value]): String =
    val (value, count) = m(0)
    s"Value: ${showval(value)}; Count: $count"

  def unitS[A](v: A): S[A] = s => (v, s)
  def bindS[A, B](m: S[A])(f: A => S[B]): S[B] = { s =>
    val (v, s1) = m(s)
    f(v)(s1)
  }
  def tickS: S[Unit] = s => ((), s + 1)

  def fetchS: S[State] = s => (s, s)

class InterpreterE extends Interpreter[Interpreter.E](using Interpreter.given_TheMonad_E):
  override protected def wrong(message: String): E[Value] =
    Interpreter.errorE(message)

class InterpreterP extends Interpreter[Interpreter.P](using Interpreter.given_TheMonad_P):
  override protected def reset(p: Position, m: P[Value]): P[Value] =
    Interpreter.resetP(p, m)
  override protected def wrong(message: String): P[Value] =
    Interpreter.errorP(message)

class InterpreterS extends Interpreter[Interpreter.S](using Interpreter.given_TheMonad_S):
  import Interpreter.{bindS, tickS, fetchS, unitS}
  import Value.Num
  override protected def doAdd(a: S[Value], b: S[Value]): S[Value] =
    bindS(tickS)(_ => super.doAdd(a, b))
  override protected def doApply(a: S[Value], b: S[Value]): S[Value] =
    bindS(tickS)(_ => super.doApply(a, b))
  override protected def count(): S[Value] =
    bindS(fetchS) { i => unitS(Num(i)) }
