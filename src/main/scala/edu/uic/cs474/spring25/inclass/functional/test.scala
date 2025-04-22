trait A:
  def getProp(): String = "A"
trait B extends A:
  override def getProp(): String = super.getProp()
trait C extends A:
  override def getProp(): String = super.getProp()
class D extends A, B, C

object Test:
  def main(args: Array[String]): Unit =
    println(D().getProp())
