import edu.uic.cs474.spring25.inclass.functional.semigroup.Semigroup

import math.*

// First implementation: handle a specific counter structure.
final case class GCounter1(counters: Map[String, Int]):
  def increment(machine: String, amount: Int): GCounter1 =
    GCounter1(counters + (machine -> (counters(machine) + amount)))

  def merge(that: GCounter1): GCounter1 =
    val inner =
      (for
        k <- this.counters.keySet
      yield k -> max(this.counters(k), that.counters(k))).toMap
    GCounter1(inner)
  end merge

  def total: Int = counters.values.sum
end GCounter1

var a = GCounter1(Map("A" -> 0, "B" -> 0))
var b = GCounter1(Map("A" -> 0, "B" -> 0))
a = a.increment("A", 1)
a
b = b.increment("B", 2)
b
a.merge(b)

// Second implmentation: handle any map
final case class GCounter[A, B](counters: Map[A, B]):
  def increment(machine: A, amount: B)(using Semigroup[A]): GCounter[A, B] = ???

  def merge(that: GCounter[A, B]): GCounter[A, B] = ???

  def total: B = ???
end GCounter

// Third implementation: handle any relation
trait GCounter3[F[_, _], A, B]:
  def increment(f: F[A, B])(a: A, b: B): F[A, B]
  def merge(first: F[A, B])(second: F[A, B]): F[A, B]
  def total(f: F[A, B]): B
