package edu.uic.cs474.spring25.inclass.functional
package casestudies.gcounter.secondattempt

import monoid.BoundedSemilattice.given
import monoid.CommutativeMonoid.given
import typeclasses.*
import monoid.*
import monoid.Monoid.*

// Second implmentation: handle any map
final case class GCounter[K, V](counters: Map[K, V]):
  def increment(machine: K, amount: V)(using Monoid[V]): GCounter[K, V] =
    GCounter(counters + (machine -> (counters.getOrElse(
      machine,
      identity[V]
    ) |+| amount)))

  def merge(that: GCounter[K, V])(using
      BoundedSemilattice[Map[K, V]],
      BoundedSemilattice[V]
  ): GCounter[K, V] =
    GCounter(this.counters |+| that.counters)
  end merge

  def total(using m: Monoid[V]): V =
    counters.values.foldLeft(m.identity)(_ |+| _)
end GCounter

object GCounter:
  given [K: Show, V: Show] => Show[GCounter[K, V]]:
    def s(f: GCounter[K, V]): String =
      s"GCounter(${f.counters.show})"

@main def main =
  def ex1 =
    println("Example 1: Map[String, Int]")
    var a = GCounter(Map("A" -> 0, "B" -> 0))
    var b = GCounter(Map("A" -> 0, "B" -> 0))
    println(a.show)
    println(b.show)

    /* Notice that there is a really significant problem here!
     *
     * We want to use the CommutativeMonoid for increment, but the
     * BoundedSemilattice instance will always be selected, because Scala will
     * favor the most specific implementation of whatever type class we request.
     * In this case, we specifically don't want the BoundedSemilattice! We want
     * to sum the ints, and not take the max!
     *
     * There are a few ways to fix this.
     * 1. We can explicitly pass the typeclass we want to increment, as I have
     * done here.
     * 2. We could break the inheritance relationship between BoundedSemilattice
     * and CommutativeMonoid. It is true that in most cases where we want a
     * monoid, the BoundedSemilattice is not really what we want, so we may want
     * it to be an independent typeclass, even though a BoundedSemilattice does
     * meet all of the requirements of a CommutativeMonoid.
     * 3. Sometimes, we can solve this issue by creating a wrapper type, for
     * which we define a different monoid. However, that would not really work
     * here, since both increment and merge need to work on a value T. */
    a = a.increment("A", 1)(using intCommutativeMonoid)
    println(a.show)
    a = a.increment("A", 3)(using intCommutativeMonoid)
    println(a.show)
    b = b.increment("B", 2)
    println(b.show)
    println(a.merge(b).show)
  end ex1

  /* This example shows how our new definition of GCounter allows us to use any
   * map, as long as we have the appropriate typeclasses defined. Here, I still
   * use String as the key, but I use Set[String] as the value, which represents
   * a set of ip addresses that have visited. */
  def ex2 =
    println("Example 2: Map[String, Set[String]]")
    /* Note that we use Set.empty[String] instead of Set() so that it is
     * appropriately typed as a Set[String] and not a Set[Nothing]
     *
     * Food for thought: Why use a Set, and not a List? Think about what a
     * bounded semilattice for List would look like. */
    var a = GCounter(Map("A" -> Set.empty[String], "B" -> Set.empty[String]))
    var b = GCounter(Map("A" -> Set.empty[String], "B" -> Set.empty[String]))
    println(a.show)
    println(b.show)
    a = a.increment("A", Set("192.168.0.1"))
    println(a.show)
    b = b.increment("B", Set("10.0.0.1", "10.0.0.2"))
    println(b.show)
    println(a.merge(b).show)
  end ex2

  /* Here, we define a GCounter where the value is itself a map, which counts
   * accesses from different IP addresses. */
  def ex3 =
    println("Example 3: Map[String, Map[String, Int]]")
    var a =
      GCounter(Map(
        "A" -> Map.empty[String, Int],
        "B" -> Map.empty[String, Int]
      ))
    var b =
      GCounter(Map(
        "A" -> Map.empty[String, Int],
        "B" -> Map.empty[String, Int]
      ))
    println(a.show)
    println(b.show)
    a = a.increment("A", Map("192.168.0.1" -> 1))
    println(a.show)
    b = b.increment("B", Map("10.0.0.1" -> 2, "10.0.0.2" -> 3))
    println(b.show)
    println(a.merge(b).show)
  end ex3

  ex1
  ex2
  ex3
end main
b
