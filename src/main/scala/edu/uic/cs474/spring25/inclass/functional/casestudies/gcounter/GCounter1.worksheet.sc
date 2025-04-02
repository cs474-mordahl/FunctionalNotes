import edu.uic.cs474.spring25.inclass.functional.monoid.*
import edu.uic.cs474.spring25.inclass.functional.semigroup.Semigroup

import math.*

// --- FIRST IMPLEMENTATION: handle a specific counter structure ---
{
  final case class GCounter(counters: Map[String, Int]):
    def increment(machine: String, amount: Int): GCounter =
      GCounter(counters + (machine -> (counters(machine) + amount)))

    def merge(that: GCounter): GCounter =
      val inner =
        (for
          k <- this.counters.keySet
        yield k -> math.max(this.counters(k), that.counters(k))).toMap
      GCounter(inner)
    end merge

    def total: Int = counters.values.sum
  end GCounter

  var a = GCounter(Map("A" -> 0, "B" -> 0))
  var b = GCounter(Map("A" -> 0, "B" -> 0))
  a = a.increment("A", 1)
  a
  b = b.increment("B", 2)
  b
  a.merge(b)
}

// That's great and all, but our GCounter only works with a specific data structure, Map[String, Int]. As programmers, we want our solutions to be as general as possible (so we don't have to write the code again!) Therefore, let's abstract our implementation to work with a map from any key (K) to any value (V).

// trait KeyValueStore[F[_, _]]:
//   def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
//   def get[K, V](f: F[K, V])(k: K): Option[V]
//   def getOrElse[K, V](f: F[K, V])(k: K, default: V): V
//   def values[K, V](f: F[K, V]): List[V]
// end KeyValueStore

// final case class GCounter3[F[_, _], A, B](counters: F[A, B]):
//   def increment(machine: A, amount: B)(using
//       CommutativeMonoid[B],
//       KeyValueStore[F]
//   ): GCounter2[A, B] =
//     GCounter2(???)

//   def merge(that: GCounter2[A, B])(using
//       Ord[B, B],
//       KeyValueStore[F]
//   ): GCounter2[A, B] =
//     val inner =
//       (for
//         k <- this.counters.keySet
//       yield k -> max(this.counters(k), that.counters(k))).toMap
//     GCounter2(inner)
//   end merge

//   def total(using m: BoundedSemilattice[B], KeyValueStore[F]): B =
//     counters.values.foldLeft(m.identity)(_ |+| _)
// end GCounter3

// var c = GCounter2(Map("A" -> 0.6, "B" -> 0.1))
// var d = GCounter2(Map("A" -> 0.6, "B" -> 0.1))
// c = c.increment("A", 2)
// c
// d = d.increment("B", 1)
// d
// val x = c.merge(d)
// x
// x.total
