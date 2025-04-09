package edu.uic.cs474.spring25.inclass.functional.casestudies.gcounter.secondattempt

import edu.uic.cs474.spring25.inclass.functional.monoid.*
import edu.uic.cs474.spring25.inclass.functional.monoid.Monoid.*
import edu.uic.cs474.spring25.inclass.functional.semigroup.Semigroup

// Second implmentation: handle any map
final case class GCounter[K, V](counters: Map[K, V]):
  def increment(machine: K, amount: V)(using Semigroup[V]): GCounter[K, V] =
    GCounter(counters + (machine -> (counters(machine) |+| amount)))

  def merge(that: GCounter[K, V])(using
      BoundedSemilattice[Map[K, V]],
      BoundedSemilattice[V]
  ): GCounter[K, V] =
    GCounter(this.counters |+| that.counters)
  end merge

  def total(using m: Monoid[V]): V =
    counters.values.foldLeft(m.identity)(_ |+| _)
end GCounter
