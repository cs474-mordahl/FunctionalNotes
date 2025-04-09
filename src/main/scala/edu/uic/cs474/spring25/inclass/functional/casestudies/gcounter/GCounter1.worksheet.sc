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
{

  var c = GCounter(Map("A" -> 0.6, "B" -> 0.1))
  var d = GCounter(Map("A" -> 0.6, "B" -> 0.1))
  c = c.increment("A", 2)
  c
  d = d.increment("B", 1)
  d
  val x = c.merge(d)
  x
  x.total
}
