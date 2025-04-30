package edu.uic.cs474.spring25.inclass.functional.derivation
package show

import magnolia1.AutoDerivation
import magnolia1.CaseClass
import magnolia1.SealedTrait

trait Show[T]:
  def show(t: T): String

object Show extends AutoDerivation[Show]:
  def join[T](caseClass: CaseClass[Show, T]): Show[T] = t =>
    val elems = caseClass.params.map: param =>
      s"${param.label}: ${param.typeclass.show(param.deref(t))}"
    elems.mkString(s"${caseClass.typeInfo.short}(", ", ", ")")
  def split[T](sealedTrait: SealedTrait[Show, T]): Show[T] = t =>
    sealedTrait.choose(t): sub =>
      sub.typeInfo.short

  given Show[String]  = (s: String) => s
  given Show[Int]     = (i: Int) => i.toString()
  given Show[Boolean] = _.toString
end Show

case class Person(
    firstName: String,
    lastName: String,
    age: Int,
    isTall: Boolean
) derives Show

enum Color derives Show:
  case Red, Blue, Yellow
  case Green(isDark: Boolean)

extension [T: Show](t: T)
  def show = summon[Show[T]].show(t)
@main
def main =
  println(Person("Austin", "Mordahl", 65, true).show)
  println(Color.Green(true).show)
