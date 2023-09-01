package fpessence

import scala.util.control.TailCalls
// import frawa.typedjson.util.FP.Functor
import scala.annotation.tailrec

/*
    See http://blog.higher-order.com/assets/trampolines.pdf
    via https://blog.rockthejvm.com/free-monad/
    and scala.util.control.TailCalls
 */

object StackFree:
  val fw = 13

enum StackFree[F[+_]: Functor, +A]:

  private case FlatMap[F[+_]: Functor, A, +B](a: StackFree[F, A], f: A => StackFree[F, B]) extends StackFree[F, B]
  case Done[F[+_]: Functor, +A](a: A)                                                      extends StackFree[F, A]
  case More[F[+_]: Functor, +A](k: F[StackFree[F, A]])                                     extends StackFree[F, A]

  final def flatMap[B](f: A => StackFree[F, B]): StackFree[F, B] =
    this match
      case x => FlatMap(x, f)

  final def map[B](f: A => B): StackFree[F, B] =
    flatMap(a => Done(f(a)))

  @tailrec
  final def resume: Either[F[StackFree[F, A]], A] =
    this match
      case Done(a) => Right(a)
      case More(k) => Left(k)
      case FlatMap(a, f) =>
        a match
          case Done(a)       => f(a).resume
          case More(k)       => Left(k.map(_ flatMap f))
          case FlatMap(b, g) => b.flatMap(x => g(x) flatMap f).resume

  // @tailrec
  // final def result: A =
  //   this.resume match {
  //     case Right(value) => value
  //     case Left(k)      => k.map(_.result)
  //   }

trait Functor[F[+_]]:
  extension [A](x: F[A]) def map[B](f: A => B): F[B]
