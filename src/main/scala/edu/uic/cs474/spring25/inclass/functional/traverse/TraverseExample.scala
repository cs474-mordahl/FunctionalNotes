package edu.uic.cs474.spring25.inclass.functional.traverse

import scala.concurrent.duration.Duration
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats.implicits.*
import cats.effect.implicits.*
import cats.instances.future.* // Import implicits for Future
import cats.effect.IO
import cats.effect.unsafe.implicits.global

object TraverseExample:

  /* However, imagine that we have a list of things that we want to write to a
   * database. */
  def doSomething(i: Int): Future[Int] =
    Future:
      Thread.sleep(2000)
      i + 1

  val li  = List(1, 2, 3, 4).map(doSomething(_))
  val li2 = li.traverse(identity)

  /* If we want to Await on inserting, we need to turn it "inside-out";
   * transform */
  /* a list of Futures into a Future of a list. We can accomplish that using
   * --- */

  // We can also use a different method to get a Future[List] from the start:
  // TODO

  // We can abstract these operations into a typeclass, called Traverse, that
  // is general for anything we want to "traverse" over.
end TraverseExample
