package edu.uic.cs474.spring25.inclass.functional
package semigroup

trait Semigroup[T]:
  def combine(t1: T, t2: T): T
  extension (t: T)
    def |+|(t2: T): T = combine(t, t2)
end Semigroup
