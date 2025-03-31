package edu.uic.cs474.spring25.inclass.functional.monoid

import edu.uic.cs474.spring25.inclass.functional.typeclasses.Semigroup

trait Monoid[T] extends Semigroup[T]:
  def identity: T

  // Addition
  given Monoid[Int] with
    def combine(t1: Int)(t2: Int): Int = t1 + t2
    def identity                       = 0

  // Lists
  given [T](using Monoid[T]): Monoid[List[T]] with
    def combine(t1: List[T], t2: List[T]): List[T] = t1.appendedAll(t2)
    def identity: List[T]                          = List()

  // NonEmptyLists

end Monoid
