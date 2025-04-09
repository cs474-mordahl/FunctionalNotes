package edu.uic.cs474.spring25.inclass.functional.monoid

import edu.uic.cs474.spring25.inclass.functional.semigroup.Semigroup

def identity[T](using Monoid[T]): T =
  summon[Monoid[T]].identity

trait Monoid[T] extends Semigroup[T]:
  def identity: T

object Monoid:
  given Monoid[Int]:
    def combine(t1: Int, t2: Int): Int = t1 + t2
    def identity                       = 0

  given Monoid[Double]:
    def combine(t1: Double, t2: Double): Double = t1 + t2
    def identity: Double                        = 0.0

  given Monoid[String]:
    def combine(t1: String, t2: String): String = t1 + t2
    def identity: String                        = ""

  given [T] => Monoid[List[T]]:
    def combine(t1: List[T], t2: List[T]): List[T] = t1 ::: t2
    def identity: List[Nothing]                    = Nil
end Monoid
