package edu.uic.cs474.spring25.inclass.functional
package monoid

import semigroup.*

/** A CommutativeMonoid is a Monoid with the additional property
  *  that its combine operation is commutative as well as associative.
  */
trait CommutativeMonoid[T] extends Monoid[T], CommutativeSemigroup[T]

object CommutativeMonoid:
  given intCommutativeMonoid: CommutativeMonoid[Int]:
    def combine(t1: Int, t2: Int): Int = t1 + t2
    def identity                       = 0

  given doubleCommutativeMonoid: CommutativeMonoid[Double]:
    def combine(t1: Double, t2: Double): Double = t1 + t2
    def identity: Double                        = 0.0
end CommutativeMonoid
