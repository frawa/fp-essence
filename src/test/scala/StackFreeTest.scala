package fpessence

import munit.FunSuite
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.annotation.tailrec

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

  test("zip ok") {
    val n      = 13
    val as     = Range.inclusive(0, n).toSeq
    val zipped = zipIndex(as)
    assertEquals(zipped.lastOption.map(_._1), Some(n))
  }

  test("zip overflow") {
    val n      = 131313
    val as     = Range.inclusive(0, n).toSeq
    val zipped = Util.TryAll(zipIndex(as))

    assertEquals(
      zipped
        .fold(ex => ex.isInstanceOf[StackOverflowError], _ => false),
      true,
      clue("stack overflow expected")
    )
  }

class MyStateTailCallsTest extends FunSuite:
  import scala.util.control.TailCalls.*

  case class MyStateTailCalls[S, +A](runS: S => TailRec[(A, S)]):
    def map[B](f: A => B) =
      MyStateTailCalls[S, B](s => runS(s).map((a, s) => (f(a), s)))
    def flatMap[B](f: A => MyStateTailCalls[S, B]) =
      MyStateTailCalls[S, B](s => {
        runS(s).flatMap((a, s) => f(a).runS(s))
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

  test("zip ok") {
    val n      = 13
    val as     = Range.inclusive(0, n).toSeq
    val zipped = zipIndex(as)
    assertEquals(zipped.lastOption.map(_._1), Some(n))
  }

  test("zip overflow") {
    val n      = 131313
    val as     = Range.inclusive(0, n).toSeq
    val zipped = Util.TryAll(zipIndex(as))

    assertEquals(
      zipped
        .fold(ex => ex.isInstanceOf[StackOverflowError], _ => false),
      true,
      clue("stack overflow expected")
    )
  }

class MyStateStackFreeTest extends FunSuite:

  enum MyStateF[S, +A]:
    case Get[S, A](f: S => A)  extends MyStateF[S, A]
    case Put[S, A](s: S, a: A) extends MyStateF[S, A]

  type MyStateF_ = [S] =>> [A] =>> MyStateF[S, A]
  given [S]: Functor[MyStateF_[S]] with
    import MyStateF._
    extension [A](x: MyStateF[S, A])
      def map[B](f: A => B): MyStateF[S, B] =
        x match
          case Get(g)    => Get(s => f(g(s)))
          case Put(s, a) => Put(s, f(a))

  type MyStateStackFree[S, +A] = StackFree[MyStateF_[S], A]

  import StackFree._
  import MyStateF._

  def pureMyStateStackFree[S, A](a: A): MyStateStackFree[S, A] = Done(a)
  def getMyStateStackFree[S]: MyStateStackFree[S, S]           = More(Get(s => Done(s)))
  def setMyStateStackFree[S](s: S): MyStateStackFree[S, Unit]  = More(Put(s, Done(())))

  @tailrec
  private def eval[S, A](s: S, x: MyStateStackFree[S, A]): A =
    x.resume match {
      case Right(value)     => value
      case Left(Put(s, xx)) => eval(s, xx)
      case Left(Get(f))     => eval(s, f(s))
    }

  private def zipIndex[A](as: Seq[A]): Seq[(Int, A)] =
    val x = as.foldLeft(
      pureMyStateStackFree[Int, Seq[(Int, A)]](Seq())
    )((acc, a) =>
      for {
        xs <- acc
        n  <- getMyStateStackFree
        _  <- setMyStateStackFree(n + 1)
      } yield (n, a) +: xs
    )
    eval(0, x).reverse

  test("zip ok"):
    val n      = 13
    val as     = Range.inclusive(0, n).toSeq
    val zipped = zipIndex(as)
    assertEquals(zipped.lastOption.map(_._1), Some(n))

  test("zip overflow"):
    val n      = 1313131
    val as     = Range.inclusive(0, n).toSeq
    val zipped = Util.TryAll(zipIndex(as))

    assertEquals(zipped.map(_.lastOption.map(_._1)).get, Some((n)))

object Util:
  object TryAll:
    def apply[K](f: => K): Try[K] =
      try {
        Success(f)
      } catch {
        case e: Throwable => Failure(e)
      }
