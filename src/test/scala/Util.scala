package fpessence

import munit.FunSuite
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.annotation.tailrec

object Util:
  object TryAll:
    def apply[K](f: => K): Try[K] =
      try {
        Success(f)
      } catch {
        case e: Throwable => Failure(e)
      }
