package fpessence

type Name = String

enum Term:
  case Var(name: Name)       extends Term
  case Con(i: Int)           extends Term
  case Add(a: Term, b: Term) extends Term
  case Lam(x: Name, e: Term) extends Term
  case App(f: Term, x: Term) extends Term

enum Value:
  case Wrong                               extends Value
  case Num(i: Int)                         extends Value
  case Fun[M[Value]](f: Value => M[Value]) extends Value

object Interpreter {
  def testTerm(term: Term): String = ???
}
