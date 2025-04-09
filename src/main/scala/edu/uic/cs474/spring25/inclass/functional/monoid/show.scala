object Show:
  trait Show[T]:
    def _show(t: T): String

  extension [T](t: T)
    def show(using s: Show[T]): String = s._show(t)

  class IntShow extends Show[Int]:
    def _show(t: Int): String = t.toString()

  given s: Show[Int]:
    def _show(i: Int): String = i.toString()

  given i: Int = 42
  def printAnswerToTheUniverse(using i: Int): Unit =
    println(i)
    
  val y: String = 3.show
end Show
