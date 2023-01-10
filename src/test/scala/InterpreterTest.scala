package fpessence

import Term._

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

class InterpreterTest extends munit.FunSuite {
  val term0 = App(Lam("x", Add(Var("x"), Var("x"))), Add(Con(10), Con(11)))
  val term1 = App(Con(1), Con(2))
  val term2 = At(Position(13), App(Con(1), Con(2)))
  val term3 = Add(Add(Con(1), Con(2)), Count)

  import Interpreter.given

  import Interpreter.I
  val interpreterI = Interpreter[I]

  test("I 42") {
    assertEquals(interpreterI.testTerm(term0), "42")
  }

  test("I wrong") {
    assertEquals(interpreterI.testTerm(term1), "<wrong>")
  }

  test("I at") {
    assertEquals(interpreterI.testTerm(term2), "<wrong>")
  }

  test("I count") {
    assertEquals(interpreterI.testTerm(term3), "<wrong>")
  }

  import Interpreter.E
  val interpreterE = new InterpreterE

  test("E 42") {
    assertEquals(interpreterE.testTerm(term0), "Success: 42")
  }

  test("E wrong") {
    assertEquals(interpreterE.testTerm(term1), "Error: should be function: 1")
  }

  test("E at") {
    assertEquals(interpreterE.testTerm(term2), "Error: should be function: 1")
  }

  test("E count") {
    assertEquals(interpreterE.testTerm(term3), "Error: cannot count")
  }

  import Interpreter.P
  val interpreterP = new InterpreterP

  test("P 42") {
    assertEquals(interpreterP.testTerm(term0), "Success: 42")
  }

  test("P wrong") {
    assertEquals(interpreterP.testTerm(term1), "Error: [0]: should be function: 1")
  }

  test("P wrong with position") {
    assertEquals(interpreterP.testTerm(term2), "Error: [13]: should be function: 1")
  }

  test("P at") {
    assertEquals(interpreterP.testTerm(term2), "Error: [13]: should be function: 1")
  }

  test("P count") {
    assertEquals(interpreterP.testTerm(term3), "Error: [0]: cannot count")
  }

  import Interpreter.S
  val interpreterS = new InterpreterS

  test("S 42") {
    assertEquals(interpreterS.testTerm(term0), "Value: 42; Count: 3")
  }

  test("S wrong") {
    assertEquals(interpreterS.testTerm(term1), "Value: <wrong>; Count: 1")
  }

  test("S at") {
    assertEquals(interpreterS.testTerm(term2), "Value: <wrong>; Count: 1")
  }

  test("S count") {
    assertEquals(interpreterS.testTerm(term3), "Value: 5; Count: 2")
  }
}
