package fpessence

import munit.FunSuite
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.annotation.tailrec

class MyStateStackFree2Test extends FunSuite:

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

  type MyStateStackFree2[S, A] = StackFree2[MyStateF_[S], A]

  import StackFree2._
  import MyStateF._

  def pureMyStateStackFree[S, A](a: A): MyStateStackFree2[S, A] = pure(a)
  def getMyStateStackFree[S]: MyStateStackFree2[S, S]           = liftM(Get(s => s))
  def setMyStateStackFree[S](s: S): MyStateStackFree2[S, Unit]  = liftM(Put(s, ()))

  import StackFreeMonad.~>
  import IdMonad.Id
  def myStateF2Id[S](zero: S): MyStateF_[S] ~> Id = new (MyStateF_[S] ~> Id):
    // TODO avoid var
    private var current = zero
    override def apply[A](fa: MyStateF_[S][A]): Id[A] = fa match
      case Get(g) =>
        Id(g(current))
      case Put(s, a) =>
        current = s
        Id(a)

  private def eval[S, A](s: S, x: MyStateStackFree2[S, A]): A =
    x.foldMap(myStateF2Id(s)).v

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

  test("zip still with overflow"):
    val n      = 1313131
    val as     = Range.inclusive(0, n).toSeq
    val zipped = Util.TryAll(zipIndex(as))

    // assertEquals(zipped.map(_.lastOption.map(_._1)).get, Some((n)))
    assertEquals(
      zipped
        .fold(ex => ex.isInstanceOf[StackOverflowError], _ => false),
      true,
      clue("stack overflow expected")
    )
