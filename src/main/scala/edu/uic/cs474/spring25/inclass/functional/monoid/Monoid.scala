package edu.uic.cs474.spring25.inclass.functional.monoid

import edu.uic.cs474.spring25.inclass.functional.semigroup.Semigroup

def identity[T](using Monoid[T]): T =
  summon[Monoid[T]].identity

/** A Monoid is a type equipped with two operations:
  * - combine: A binary operation that maps two values to one.
  * - identity: Retrieves the identity for the combine operation.
  *
  * A Monoid must obey three laws:
  * 1. Associativity: forAll a, b, c:
  *        combine(combine(a, b), c) == combine(a, combine(b, c))
  * 2. Left Identity: forAll a:
  *  combine(identity, a) == a
  * 3. Right Identity: forAll a:
  *  combine(a, identity) == a
  */
trait Monoid[T] extends Semigroup[T]:
  def identity: T

object Monoid:
  given Monoid[String]:
    def combine(t1: String, t2: String): String = t1 + t2
    def identity: String                        = ""

  given [T] => Monoid[List[T]]:
    def combine(t1: List[T], t2: List[T]): List[T] = t1 ::: t2
    def identity: List[Nothing]                    = Nil
end Monoid
