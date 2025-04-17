import edu.uic.cs474.spring25.inclass.functional.monad.Monad
import edu.uic.cs474.spring25.inclass.functional.monad.Monad.*
import edu.uic.cs474.spring25.inclass.functional.util.MyEither
import edu.uic.cs474.spring25.inclass.functional.util.MyEither.*

// ***** EITHER PRACTICE *****
object EitherPractice:
  @main def main: Unit =
    val x: MyEither[String, Int] = 0.asRight[String]
    val y = x.flatMap(i =>
      if i == 0 then "Cannot divide by zero".asLeft[Int]
      else (100 / i).asRight[String]
    ).flatMap(i => (i * 4).asRight[String])
    println(y) // Left("Cannot divide by zero")

    // We can also use the for-comprehension, since we have both flatMap and
    //  map defined.
    val curried = divide(100)
    val z =
      for
        i <- curried(20)
        j <- curried(i)
      yield j

    println(z)
  end main

  def divide(i: Int)(j: Int): MyEither[String, Int] =
    if j == 0 then "Cannot divide by zero".asLeft[Int]
    else (i / j).asRight[String]
end EitherPractice
