package fpessence

import Term._

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

class InterpreterTest extends munit.FunSuite {
  val term0 = App(Lam("x", Add(Var("x"), Var("x"))), Add(Con(10), Con(11)))
  val term1 = App(Con(1), Con(2))

  import Interpreter.given

  import Interpreter.I
  val interpreterI = Interpreter[I]

  test("I 42") {
    assertEquals(interpreterI.testTerm(term0), "42")
  }

  test("I wrong") {
    assertEquals(interpreterI.testTerm(term1), "<wrong>")
  }

  import Interpreter.E
  val interpreterE = new InterpreterE

  test("E 42") {
    assertEquals(interpreterE.testTerm(term0), "Success: 42")
  }

  test("E wrong") {
    assertEquals(interpreterE.testTerm(term1), "Error: should be function: 1")
  }

  // test("P 42") {
  //   import InterpreterP._
  //   assertEquals(testTerm(term0), "Success: 42")
  // }

  // test("P wrong") {
  //   import InterpreterP._
  //   assertEquals(testTerm(term1), "Error: should be function: 1")
  // }

}
