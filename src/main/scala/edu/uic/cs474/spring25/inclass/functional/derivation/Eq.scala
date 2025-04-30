package edu.uic.cs474.spring25.inclass.functional.derivation
package eq

import magnolia1.AutoDerivation
import magnolia1.CaseClass
import magnolia1.SealedTrait

trait Eq[T]:
  def equals(t1: T, t2: T): Boolean

object Eq extends AutoDerivation[Eq]:
  def join[T](caseClass: CaseClass[Eq, T]): Eq[T] = (t1, t2) =>
    caseClass.params.forall: param =>
      param.typeclass.equals(param.deref(t1), param.deref(t2))

  def split[T](sealedTrait: SealedTrait[Eq, T]): Eq[T] = (t1, t2) =>
    sealedTrait.choose(t1): sub1 =>
      sealedTrait.choose(t2): sub2 =>
        if sub1.typeInfo == sub2.typeInfo then
          sub1.typeclass.equals(sub1.value, sub1.cast(t2))
        else
          false

  given Eq[String]  = _ == _
  given Eq[Int]     = _ == _
  given Eq[Boolean] = _ == _
end Eq

case class Person(
    name: String,
    age: Int,
    isTall: Boolean
) derives Eq

enum Color derives Eq:
  case Red, Blue, Yellow
  case Green(isDark: Boolean)

extension [T: Eq](t1: T)
  def =*=(t2: T) = summon[Eq[T]].equals(t1, t2)

@main
def main =
  println(Person("Austin", 65, false) =*= Person("Austin", 65, true))
  println(Color.Red =*= Color.Red)
  println(Color.Red =*= Color.Red)
end main
