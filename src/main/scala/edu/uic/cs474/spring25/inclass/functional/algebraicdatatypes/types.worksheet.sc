def add1(x: Int, y: Int): Int                       = x + y
def myUsing[T <: { def close(): Unit }](t: T): Unit = ???

def countToTen(): List[Int] =
  var x: Int = 0
  val result = scala.collection.mutable.ListBuffer[Int]()
  for
    y <- 1 until 10
  do
    x = x + 1
    result.append(x)
  end for
  result.toList
end countToTen
