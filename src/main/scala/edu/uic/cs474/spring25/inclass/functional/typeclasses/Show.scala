package edu.uic.cs474.spring25.inclass.functional
package typeclasses

import MyOption.{MyNone, MySome}

trait Show[T]:
  def s(t: T): String

object Show:
  extension [T: Show](t: T)
    def show: String = summon[Show[T]].s(t)

  // All AnyVals can just use toString
  given [T <: AnyVal] => Show[T]:
    def s(t: T): String = t.toString()

  given Show[String]:
    def s(t: String): String = t

  given Show[Int]:
    def s(t: Int): String = t.toString()

  given [K: Show, V: Show] => Show[Map[K, V]]:
    def s(t: Map[K, V]): String =
      s"Map(${t.toSeq.map((k, v) => s"${k.show} -> ${v.show}").mkString(", ")})"

  given [T: Show] => Show[Set[T]]:
    def s(t: Set[T]): String =
      s"Set(${t.map(_.show).mkString(", ")})"

  given [T: Show] => Show[MyOption[T]]:
    def s(t: MyOption[T]): String = t match
      case MySome(v) => s"MySome(${v.show})"
      case MyNone    => "MyNone"
end Show
