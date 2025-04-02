package edu.uic.cs474.spring25.inclass

given Show[Double]:
  def _show(t: Double): String = t.toString()

given [T](using Show[T]): Show[Option[T]] with
  def _show(t: Option[T]): String =
    t match
      case Some(t) => s"Some(${t.show})"
      case None    => "None"
end given

given [T](using Show[T]): Show[List[T]] with
  def _show(t: List[T]): String =
    t match
      case h :: t => s"${h.show} :: ${t.show}"
      case Nil    => "Nil"
end given

object Application:
  extension [T](t: T)
    def show(using s: Show[T]) = s._show(t)

  def main(args: Array[String]): Unit =
    val y: List[Option[Double]] = List(Some(8.8), None, Some(1.0), Some(-1.4))
    println(y.show)
  end main

end Application
