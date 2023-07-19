package fpessence

import munit.FunSuite
import scala.util.Try
import scala.util.Failure
import scala.util.Success

class MyStateTest extends FunSuite:

  case class MyState[S, +A](runS: S => (A, S)):
    def map[B](f: A => B) =
      MyState[S, B](s => {
        val (a, s1) = runS(s)
        (f(a), s1)
      })
    def flatMap[B](f: A => MyState[S, B]) =
      MyState[S, B](s => {
        val (a, s1) = runS(s)
        f(a).runS(s1)
      })

  def getMyState[S]: MyState[S, S]           = MyState(s => (s, s))
  def setMyState[S](s: S): MyState[S, Unit]  = MyState(_ => ((), s))
  def pureMyState[S, A](a: A): MyState[S, A] = MyState(s => (a, s))

  private def zipIndex[A](as: Seq[A]): Seq[(Int, A)] =
    as.foldLeft(
      pureMyState[Int, Seq[(Int, A)]](Seq())
    )((acc, a) =>
      for {
        xs <- acc
        n  <- getMyState
        _  <- setMyState(n + 1)
      } yield (n, a) +: xs
    ).runS(0)
      ._1
      .reverse

  test("zip ok"):
    val as     = Range(0, 13).toSeq
    val zipped = zipIndex(as)
    assertEquals(zipped.lastOption.map(_._1), Some((12)))

  

  test("zip overflow"):
    val as     = Range(0, 131313).toSeq
    val zipped = Util.TryAll(zipIndex(as))

    assertEquals(
      zipped
        .fold(ex => ex.isInstanceOf[StackOverflowError], _ => false),
      true,
      clue("stack overlflow expected")
    )


class MyStateTailCallsTest extends FunSuite:
  import scala.util.control.TailCalls.*

  case class MyStateTailCalls[S, +A](runS: S => TailRec[(A, S)]):
    def map[B](f: A => B) =
      MyStateTailCalls[S, B](s => 
        runS(s).map((a,s) => (f(a),s))
      )
    def flatMap[B](f: A => MyStateTailCalls[S, B]) =
      MyStateTailCalls[S, B](s => {
        runS(s).flatMap((a,s) => 
          f(a).runS(s)
        )
      })   

  def pureMyStateTailCalls[S, A](a: A): MyStateTailCalls[S, A] = MyStateTailCalls(s => done((a, s)))
  def setMyStateTailCalls[S](s: S): MyStateTailCalls[S, Unit]  = MyStateTailCalls(_ => done(((), s)))
  def getMyStateTailCalls[S]: MyStateTailCalls[S, S]           = MyStateTailCalls(s => done((s, s)))

  private def zipIndex[A](as: Seq[A]): Seq[(Int, A)] =
    as.foldLeft(
      pureMyStateTailCalls[Int, Seq[(Int, A)]](Seq())
    )((acc, a) =>
      for {
        xs <- acc
        n  <- getMyStateTailCalls
        _  <- setMyStateTailCalls(n + 1)
      } yield (n, a) +: xs
    ).runS(0)
      .result
      ._1
      .reverse


  test("zip ok"):
    val as     = Range(0, 13).toSeq
    val zipped = zipIndex(as)
    assertEquals(zipped.lastOption.map(_._1), Some((12)))

  test("zip overflow"):
    val as     = Range(0, 131313).toSeq
    val zipped = Util.TryAll(zipIndex(as))

    assertEquals(
      zipped
        .fold(ex => ex.isInstanceOf[StackOverflowError], _ => false),
      true,
      clue("stack overlflow expected")
    )


object Util:
  object TryAll:
    def apply[K](f: => K): Try[K] =
      try {
        Success(f)
      } catch {
        case e: Throwable => Failure(e)
      }