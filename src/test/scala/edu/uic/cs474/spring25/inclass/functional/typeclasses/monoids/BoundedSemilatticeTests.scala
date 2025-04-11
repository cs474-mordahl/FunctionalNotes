package edu.uic.cs474.spring25.inclass.functional.monoid
import munit.FunSuite
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class BoundedSemilatticeTests extends FunSuite, ScalaCheckSuite:
  property("Associative"):
    forAll: (a: Int, b: Int, c: Int) =>
      val bs = summon[BoundedSemilattice[Int]]
      bs.combine(a, bs.combine(b, c)) ==
        bs.combine(bs.combine(a, b), c)

  property("Commutative"):
    forAll: (a: Int, b: Int) =>
      val bs = summon[BoundedSemilattice[Int]]
      bs.combine(a, b) == bs.combine(b, a)

  property("Idempotent"):
    forAll: (a: Int, b: Int) =>
      val bs   = summon[BoundedSemilattice[Int]]
      val comb = bs.combine(a, b)
      comb == bs.combine(comb, comb)

end BoundedSemilatticeTests
