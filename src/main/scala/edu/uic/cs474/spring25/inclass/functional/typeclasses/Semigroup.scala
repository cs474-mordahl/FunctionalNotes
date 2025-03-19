package edu.uic.cs474.spring25.inclass.functional.typeclasses

trait Semigroup[T]:
  def combine(t1: T)(t2: T): T
