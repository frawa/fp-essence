package fpessence

import Term._

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

class InterpreterTest extends munit.FunSuite {

  val term42      = App(Lam("x", Add(Var("x"), Var("x"))), Add(Con(10), Con(11)))
  val termWrong   = App(Con(1), Con(2))
  val termWrongAt = At(Position(13), App(Con(1), Con(2)))
  val termCount   = Add(Add(Con(1), Con(2)), Count)
  val termOut     = Add(Out("a", Con(41)), Out("b", Con(1)))

  import Interpreter.given

  {
    import Interpreter.I
    val interpreterI = Interpreter[I]

    test("I 42") {
      assertEquals(interpreterI.testTerm(term42), "42")
    }

    test("I wrong") {
      assertEquals(interpreterI.testTerm(termWrong), "<wrong>")
    }

    test("I at") {
      assertEquals(interpreterI.testTerm(termWrongAt), "<wrong>")
    }

    test("I count") {
      assertEquals(interpreterI.testTerm(termCount), "<wrong>")
    }

    test("I out") {
      assertEquals(interpreterI.testTerm(termOut), "<wrong>")
    }
  }

  {
    import Interpreter.E
    val interpreterE = Interpreter[E]

    test("E 42") {
      assertEquals(interpreterE.testTerm(term42), "Success: 42")
    }

    test("E wrong") {
      assertEquals(interpreterE.testTerm(termWrong), "Error: should be function: 1")
    }

    test("E at") {
      assertEquals(interpreterE.testTerm(termWrongAt), "Error: should be function: 1")
    }

    test("E count") {
      assertEquals(interpreterE.testTerm(termCount), "Error: cannot count")
    }

    test("E out") {
      assertEquals(interpreterE.testTerm(termOut), "Error: cannot out")
    }
  }

  {
    import Interpreter.P
    val interpreterP = Interpreter[P]

    test("P 42") {
      assertEquals(interpreterP.testTerm(term42), "Success: 42")
    }

    test("P wrong") {
      assertEquals(interpreterP.testTerm(termWrong), "Error: [0]: should be function: 1")
    }

    test("P wrong with position") {
      assertEquals(interpreterP.testTerm(termWrongAt), "Error: [13]: should be function: 1")
    }

    test("P at") {
      assertEquals(interpreterP.testTerm(termWrongAt), "Error: [13]: should be function: 1")
    }

    test("P count") {
      assertEquals(interpreterP.testTerm(termCount), "Error: [0]: cannot count")
    }

    test("P cout") {
      assertEquals(interpreterP.testTerm(termOut), "Error: [0]: cannot out")
    }
  }

  {
    import Interpreter.S
    val interpreterS = Interpreter[S]

    test("S 42") {
      assertEquals(interpreterS.testTerm(term42), "Value: 42; Count: 3")
    }

    test("S wrong") {
      assertEquals(interpreterS.testTerm(termWrong), "Value: <wrong>; Count: 1")
    }

    test("S at") {
      assertEquals(interpreterS.testTerm(termWrongAt), "Value: <wrong>; Count: 1")
    }

    test("S count") {
      assertEquals(interpreterS.testTerm(termCount), "Value: 5; Count: 2")
    }

    test("S out") {
      assertEquals(interpreterS.testTerm(termOut), "Value: <wrong>; Count: 1")
    }
  }

  {
    import Interpreter.O
    val interpreterO = Interpreter[O]

    test("O 42") {
      assertEquals(interpreterO.testTerm(term42), "Output:  Value: 42")
    }

    test("O wrong") {
      assertEquals(interpreterO.testTerm(termWrong), "Output:  Value: <wrong>")
    }

    test("O at") {
      assertEquals(interpreterO.testTerm(termWrongAt), "Output:  Value: <wrong>")
    }

    test("O count") {
      assertEquals(interpreterO.testTerm(termCount), "Output:  Value: <wrong>")
    }

    test("O out") {
      assertEquals(interpreterO.testTerm(termOut), "Output: a:41;b:1 Value: 42")
    }
  }
}
