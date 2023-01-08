package fpessence

import Term._

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

class InterpreterTest extends munit.FunSuite {
  val term0 = App(Lam("x", Add(Var("x"), Var("x"))), Add(Con(10), Con(11)))
  val term1 = App(Con(1), Con(2))

  test("I 42") {
    import InterpreterI._
    assertEquals(testTerm(term0), "42")
  }

  test("I wrong") {
    import InterpreterI._
    assertEquals(testTerm(term1), "<wrong>")
  }

  test("E 42") {
    import InterpreterE._
    assertEquals(testTerm(term0), "Success: 42")
  }

  test("E wrong") {
    import InterpreterE._
    assertEquals(testTerm(term1), "Error: should be function: 1")
  }

}
