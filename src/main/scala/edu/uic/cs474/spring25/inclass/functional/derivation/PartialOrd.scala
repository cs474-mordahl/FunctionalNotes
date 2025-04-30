package edu.uic.cs474.spring25.inclass.functional.derivation

package partialOrder

import magnolia1.AutoDerivation
import magnolia1.CaseClass
import magnolia1.SealedTrait
import cats.effect.kernel.Par

trait PartialOrd[T]:
  def compare(t1: T, t2: T): PartialOrd.Result

object PartialOrd extends AutoDerivation[PartialOrd]:

  def join[T](caseClass: CaseClass[PartialOrd, T]): PartialOrd[T] = (t1, t2) =>
    val result = caseClass.params.map: param =>
      param.typeclass.compare(param.deref(t1), param.deref(t2))

    val resSet = result.toSet
    if resSet.size == 1 then resSet.head
    else Result.UNDEF
  def split[T](sealedTrait: SealedTrait[PartialOrd, T]): PartialOrd[T] =
    (t1, t2) =>
      sealedTrait.choose(t1): sub1 =>
        sealedTrait.choose(t2): sub2 =>
          if sub1.typeInfo == sub2.typeInfo then PartialOrd.Result.EQ
          else PartialOrd.Result.UNDEF
  enum Result:
    case LT, GT, EQ, UNDEF

  given PartialOrd[Int] = (t1, t2) =>
    if t1 == t2 then PartialOrd.Result.EQ
    else if t1 < t2 then PartialOrd.Result.LT
    else PartialOrd.Result.GT
end PartialOrd

case class Tuple[T](t1: T, t2: T) derives PartialOrd

enum Color derives PartialOrd:
  case Red, Blue

extension [T: PartialOrd](t1: T)
  def <<(t2: T) = summon[PartialOrd[T]].compare(t1, t2)

@main
def main =
  println(Tuple(3, 3) << Tuple(2, 2))
  println(Color.Red << Color.Red)
