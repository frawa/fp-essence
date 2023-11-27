package fpessence

import scala.annotation.tailrec

/*
    See https://blog.rockthejvm.com/free-monad/
 */

object StackFreeMonad:
  trait Monad[M[_]]:
    def pure[A](a: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  object Monad:
    def apply[M[_]](using monad: Monad[M]): Monad[M] = monad

  trait ~>[F[_], G[_]]:
    def apply[A](fa: F[A]): G[A]

trait StackFree2[M[_], A]:
  import StackFreeMonad.*
  import StackFree2.*
  def flatMap[B](f: A => StackFree2[M, B]): StackFree2[M, B] = this match
    case FlatMap(fa, f1) => fa.flatMap(a => f1(a).flatMap(f))
    case _               => FlatMap(this, f)
  def map[B](f: A => B): StackFree2[M, B] = flatMap(a => pure(f(a)))

  // @tailrec
  final def foldMap[G[_]: Monad](natTrans: M ~> G): G[A] = this match
    case Pure(a)     => Monad[G].pure(a)
    case Suspend(ma) => natTrans.apply(ma)
    case FlatMap(fa, f) => // need a G[B]
      Monad[G].flatMap(fa.foldMap(natTrans))(a => f(a).foldMap(natTrans))

object StackFree2:
  def pure[M[_], A](a: A): StackFree2[M, A]      = Pure(a)
  def liftM[M[_], A](ma: M[A]): StackFree2[M, A] = Suspend(ma)

  case class Pure[M[_], A](a: A)                                                 extends StackFree2[M, A]
  case class FlatMap[M[_], A, B](fa: StackFree2[M, A], f: A => StackFree2[M, B]) extends StackFree2[M, B]
  case class Suspend[M[_], A](ma: M[A])                                          extends StackFree2[M, A]

object IdMonad:
  case class Id[A](v: A)

  import StackFreeMonad.Monad
  given idMonad: Monad[Id] with
    def pure[A](a: A): Id[A]                           = Id(a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma.v)
