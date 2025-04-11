package edu.uic.cs474.spring25.inclass.functional.typeclasses

trait Show[T]:
  def s(t: T): String
extension [T](t: T)
  def show(using s: Show[T]): String = s.s(t)

object Show:
  // All AnyVals can just use toString
  given [T <: AnyVal] => Show[T]:
    def s(t: T): String = t.toString()

  given Show[String]:
    def s(t: String): String = t

  given [K: Show, V: Show] => Show[Map[K, V]]:
    def s(t: Map[K, V]): String =
      s"Map(${t.toSeq.map((k, v) => s"${k.show} -> ${v.show}").mkString(", ")})"

  given [T: Show] => Show[Set[T]]:
    def s(t: Set[T]): String =
      s"Set(${t.map(_.show).mkString(", ")})"
end Show
