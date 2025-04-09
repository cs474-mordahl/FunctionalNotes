package edu.uic.cs474.spring25.inclass.functional.casestudies.gcounter.firstattempt
import math.*

// --- FIRST IMPLEMENTATION: handle a specific counter structure ---
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

@main def main(): Unit =
  var a = GCounter(Map("A" -> 0, "B" -> 0))
  var b = GCounter(Map("A" -> 0, "B" -> 0))
  a = a.increment("A", 1)
  a
  b = b.increment("B", 2)
  b
  a.merge(b)
end main
