import edu.uic.cs474.spring25.inclass.functional.typeclasses.MyEither
import edu.uic.cs474.spring25.inclass.functional.typeclasses.MyEither.*
import edu.uic.cs474.spring25.inclass.functional.monad.Monad
import edu.uic.cs474.spring25.inclass.functional.monad.Monad.*

val x = MyEither.asRight[String, Int](0)
x.flatMap(i =>
  if i == 0 then MyEither.asLeft[String, Int]("Cannot divide by zero")
  else MyEither.asRight[String, Int](100 / i)
).flatMap(i => MyEither.asRight[String, Int](i * 4))
