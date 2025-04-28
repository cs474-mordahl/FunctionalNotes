package edu.uic.cs474.spring25.inclass.functional.applicative

import cats.effect.IO
import cats.implicits.*
import scala.concurrent.duration.Duration
import cats.effect.unsafe.implicits.global
import cats.Foldable

// Imagine that I have a method that produces some IO effect
//  (e.g., writing to console).
def writeToDb(s: String, delay: Long): IO[Unit] =
  IO:
    Thread.sleep(delay)
    println(s)
object MapExample:
  def main(args: Array[String]): Unit =
    // Writing one thing to the db is a cinch.
    val insert1 =
      writeToDb("first one", Duration(3, "sec").toMillis)

    val insert2 =
      writeToDb("second one", Duration(2, "sec").toMillis)

    val insert3 =
      writeToDb("third one", Duration(1, "sec").toMillis)

    val f = (insert1, insert2, insert3).parMapN((b1, b2, b3) =>
      IO:
        println("Done!")
    )

    f.unsafeRunSync()
  end main
end MapExample
