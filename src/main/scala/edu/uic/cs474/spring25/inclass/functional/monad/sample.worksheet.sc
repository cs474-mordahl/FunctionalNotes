import edu.uic.cs474.spring25.inclass.functional.monad.Monad
import edu.uic.cs474.spring25.inclass.functional.monad.Monad.*
import edu.uic.cs474.spring25.inclass.functional.util.MyEither
import edu.uic.cs474.spring25.inclass.functional.util.MyEither.*

def initialValue = Some(4)

def timesByTwo(i: Int): Option[Int] =
  if i % 2 != 0 then None else Some(i * 2)
def divideByNine(i: Int): Option[Int] =
  if i > 100 then None else Some(i / 9)
def plusThree(i: Int): Option[Int] =
  if i < 5 then None else Some(i + 3)

def doAllThree(i: Int)(using Monad[Option]): Option[Int] =
  /* This shows how we can use for-comprehensions to simplify many nested calls
   * to flatMap. */
  for
    e <- timesByTwo(i)
    g <- divideByNine(e)
    h <- plusThree(g)
    j <- timesByTwo(h)
  yield j

// This is exactly equivalent to the function above; this is how for-comprehensions turn into
// flatMap and map calls.
def doAllThreeEquivalent(i: Int)(using m: Monad[Option]): Option[Int] =
  timesByTwo(i).flatMap(e =>
    divideByNine(e).flatMap(g =>
      plusThree(g).flatMap(h => timesByTwo(h).map(j => j))
    )
  )

// ***** EITHER PRACTICE *****
val x: MyEither[String, Int] = 0.asRight[String]
val y = x.flatMap(i =>
  if i == 0 then "Cannot divide by zero".asLeft[Int]
  else (100 / i).asRight[String]
).flatMap(i => (i * 4).asRight[String])
println(y)
