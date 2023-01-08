package fpessence

import Term._
import Value._
import Interpreter.testTerm

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

class InterpreterTest extends munit.FunSuite {
  test("42") {
    val term0 = App(Lam("x", Add(Var("x"), Var("x"))), Add(Con(10), Con(11)))
    assertEquals(testTerm(term0), "42")
  }
}
