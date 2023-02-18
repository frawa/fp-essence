package fpessence

// TODO opaque
type Name = String

opaque type Position = Int
object Position:
  val pos0: Position          = 0
  def apply(p: Int): Position = p

enum Term:
  case Var(name: Name)             extends Term
  case Con(i: Int)                 extends Term
  case Add(a: Term, b: Term)       extends Term
  case Lam(x: Name, e: Term)       extends Term
  case App(f: Term, t: Term)       extends Term
  case At(p: Position, t: Term)    extends Term
  case Count                       extends Term
  case Out(label: String, t: Term) extends Term

trait TheMonad[M[_]]:
  def showM(m: M[Value]): String
  def unitM[A](v: A): M[A]
  def bindM[A, B](m: M[A])(f: A => M[B]): M[B]
  def wrongM(message: String): M[Value]
  def resetM(p: Position, m: M[Value]): M[Value]
  def countM(): M[Value]
  def outM(label: String, m1: M[Value]): M[Value]
  def addM(a: M[Value], b: M[Value]): M[Value]
  def applyM(a: M[Value], b: M[Value]): M[Value]

  extension [A](m: M[A])
    def flatMap[B](f: A => M[B]): M[B] = bindM(m)(f)
    def map[B](f: A => B): M[B]        = bindM(m)(a => unitM(f(a)))

enum Value:
  case Wrong                               extends Value
  case Num(i: Int)                         extends Value
  case Fun[M[Value]](f: Value => M[Value]) extends Value

class Interpreter[M[_]: TheMonad]:
  protected val m = summon[TheMonad[M]]

  final def testTerm(term: Term): String = m.showM(interp(term, Seq()))

  private type Environment = Seq[(Name, Value)]

  import Term._
  import Value._

  protected def showval(v: Value): String = v match
    case Wrong  => "<wrong>"
    case Num(i) => s"$i"
    case Fun(f) => "<function>"

  private def interp(t: Term, e: Environment): M[Value] = t match
    case Var(name)     => lookup(name, e)
    case Con(i)        => m.unitM(Num(i))
    case Add(a, b)     => m.addM(interp(a, e), interp(b, e))
    case Lam(x, t)     => m.unitM(Fun(xx => interp(t, (x, xx) +: e)))
    case App(f, t)     => m.applyM(interp(f, e), interp(t, e))
    case At(p, t)      => m.resetM(p, interp(t, e))
    case Count         => m.countM()
    case Out(label, t) => m.outM(label, interp(t, e))

  private def lookup(name: Name, e: Environment): M[Value] = e match
    case Nil            => m.wrongM(s"unbound variable: $name")
    case (n, v) :: rest => if n == name then m.unitM(v) else lookup(name, rest)

object Interpreter:
  import Value._

  // --- I

  type I[A] = A

  given TheMonad[I] with
    def showM(m: I[Value]): String                  = showval(m)
    def unitM[A](v: A): I[A]                        = v
    def bindM[A, B](m: I[A])(f: A => I[B]): I[B]    = f(m)
    def wrongM(message: String): I[Value]           = unitM(Wrong)
    def resetM(p: Position, m: I[Value]): I[Value]  = m
    def countM()                                    = wrongM("cannot count")
    def outM(label: String, m1: I[Value]): I[Value] = wrongM("cannot cout")
    def addM(a: I[Value], b: I[Value]): I[Value]    = doAddM(a, b)
    def applyM(a: I[Value], b: I[Value]): I[Value]  = doApplyM(a, b)

  private def doAddM[M[_]: TheMonad](a: M[Value], b: M[Value]): M[Value] =
    // m.bindM(a) { a => m.bindM(b) { b => doAdd(a, b) } }
    for
      aa <- a
      bb <- b
      r  <- add(aa, bb)
    yield r

  private def add[M[_]: TheMonad](a: Value, b: Value): M[Value] = (a, b) match
    case (Num(a), Num(b)) => summon[TheMonad[M]].unitM(Num(a + b))
    case _                => summon[TheMonad[M]].wrongM(s"should be numbers: ${showval(a)}, ${showval(b)}")

  private def doApplyM[M[_]: TheMonad](f: M[Value], v: M[Value]): M[Value] =
    // m.bindM(f) { f => m.bindM(v) { v => doApply(f, v) } }
    for
      ff <- f
      vv <- v
      r  <- doApply(ff, vv)
    yield r

  private def doApply[M[_]: TheMonad](f: Value, v: Value): M[Value] = f match
    case Fun[M](f) => f(v) // ff.f(v)
    case _         => summon[TheMonad[M]].wrongM(s"should be function: ${showval(f)}")

  def showval(v: Value): String = v match
    case Wrong  => "<wrong>"
    case Num(i) => s"$i"
    case Fun(f) => "<function>"

  def showpos(p: Position): String =
    s"[$p]"

  // --- E

  enum E[A]:
    case Success(a: A)          extends E[A]
    case Error(message: String) extends E[A]

  given TheMonad[E] with
    def showM(m: E[Value]): String                  = showE(m)
    def unitM[A](v: A): E[A]                        = unitE(v)
    def bindM[A, B](m: E[A])(f: A => E[B]): E[B]    = bindE(m)(f)
    def wrongM(message: String): E[Value]           = errorE(message)
    def resetM(p: Position, m: E[Value]): E[Value]  = m
    def countM()                                    = wrongM("cannot count")
    def outM(label: String, m1: E[Value]): E[Value] = wrongM("cannot out")
    def addM(a: E[Value], b: E[Value]): E[Value]    = doAddM(a, b)
    def applyM(a: E[Value], b: E[Value]): E[Value]  = doApplyM(a, b)

  private def showE(m: E[Value]): String = m match
    case E.Success(a) => s"Success: ${showval(a)}"
    case E.Error(m)   => s"Error: ${m}"

  private def unitE[A](a: A): E[A] = E.Success(a)
  def errorE[A](m: String): E[A]   = E.Error(m)
  private def bindE[A, B](m: E[A])(f: A => E[B]): E[B] = m match
    case E.Success(a) => f(a)
    case E.Error(m)   => E.Error(m)

  // --- P

  type P[A] = Position => E[A]

  given TheMonad[P] with
    def showM(m: P[Value]): String                  = showP(m)
    def unitM[A](v: A): P[A]                        = unitP(v)
    def bindM[A, B](m: P[A])(f: A => P[B]): P[B]    = bindP(m)(f)
    def wrongM(m: String): P[Value]                 = errorP(m)
    def resetM(p: Position, m: P[Value]): P[Value]  = resetP(p, m)
    def countM()                                    = wrongM("cannot count")
    def outM(label: String, m1: P[Value]): P[Value] = wrongM("cannot out")
    def addM(a: P[Value], b: P[Value]): P[Value]    = doAddM(a, b)
    def applyM(a: P[Value], b: P[Value]): P[Value]  = doApplyM(a, b)

  private def showP(m: P[Value]): String = showE(m(Position.pos0))
  def errorP[A](m: String): P[A]         = p => errorE(s"${showpos(p)}: $m")

  private def unitP[A](v: A): P[A] = p => unitE(v)
  private def bindP[A, B](m: P[A])(f: A => P[B]): P[B] = p =>
    // bindE(m(p))(x => f(x)(p))
    for
      x <- m(p)
      r <- f(x)(p)
    yield r

  def resetP[A](p: Position, m: P[A]): P[A] = q => m(p)

  // --- S

  type State = Int
  type S[A]  = State => (A, State)

  given TheMonad[S] with
    def showM(m: S[Value]): String                  = showS(m)
    def unitM[A](v: A): S[A]                        = unitS(v)
    def bindM[A, B](m: S[A])(f: A => S[B]): S[B]    = bindS(m)(f)
    def wrongM(m: String): S[Value]                 = unitS(Wrong)
    def resetM(p: Position, m: S[Value]): S[Value]  = m
    def countM()                                    = for i <- fetchS yield Num(i)
    def outM(label: String, m1: S[Value]): S[Value] = wrongM("cannot out")
    def addM(a: S[Value], b: S[Value]): S[Value] =
      // m.bindM(tickS)(_ => super.doAdd(a, b))
      for
        _ <- tickS
        r <- doAddM(a, b)
      yield r
    def applyM(a: S[Value], b: S[Value]): S[Value] =
      // m.bindM(tickS)(_ => super.doApply(a, b))
      for
        _ <- tickS
        r <- doApplyM(a, b)
      yield r

  private def showS(m: S[Value]): String =
    val (value, count) = m(0)
    s"Value: ${showval(value)}; Count: $count"

  def unitS[A](v: A): S[A] = s => (v, s)
  def bindS[A, B](m: S[A])(f: A => S[B]): S[B] = s =>
    val (v, s1) = m(s)
    f(v)(s1)

  def tickS: S[Unit] = s => ((), s + 1)

  def fetchS: S[State] = s => (s, s)

  // --- O

  type O[A] = (Seq[String], A)

  given TheMonad[O] with
    def showM(m: O[Value]): String                 = showO(m)
    def unitM[A](v: A): O[A]                       = unitO(v)
    def bindM[A, B](m: O[A])(f: A => O[B]): O[B]   = bindO(m)(f)
    def wrongM(m: String): O[Value]                = unitO(Wrong)
    def resetM(p: Position, m: O[Value]): O[Value] = m
    def countM()                                   = wrongM("cannot count")
    def outM(label: String, m1: O[Value]): O[Value] =
      for
        vv <- m1
        x  <- outO(label, vv)
      yield vv
    def addM(a: O[Value], b: O[Value]): O[Value]   = doAddM(a, b)
    def applyM(a: O[Value], b: O[Value]): O[Value] = doApplyM(a, b)

  private def showO(m: O[Value]): String =
    val (lines, v) = m
    s"Output: ${lines.mkString(";")} Value: ${showval(v)}"

  private def unitO[A](v: A): O[A] =
    (Seq(), v)

  private def bindO[A, B](m: O[A])(f: A => O[B]): O[B] =
    val (lines, a)  = m
    val (lines2, b) = f(a)
    (lines ++ lines2, b)

  def outO(label: String, v: Value): O[Unit] = (Seq(s"$label:${showval(v)}"), ())

end Interpreter
