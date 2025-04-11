package edu.uic.cs474.spring25.inclass.functional
package casestudies.gcounter.firstattempt

import typeclasses.*
import math.*

/** Our first attempt at creating a GCounter.
  * We make `counters` a concrete type; specifically,
  * [[Map[String, Int]]].
  *
  * @param counters The map to use as the internal container.
  */
final case class GCounter(counters: Map[String, Int]):
  /** Registers `amount` new visitors for machine `machine`.
    *
    * @param machine
    * @param amount
    * @return A new [[GCounter]] with the updated value.
    */
  def increment(machine: String, amount: Int): GCounter =
    GCounter(counters + (machine -> (counters(machine) + amount)))

  /** Merges two [[GCounter]]s together, using [[math.max]].
    *
    * @param that The other GCounter to merge.
    * @return A new GCounter, with the values from `this` and `that` merged
    *    together.
    */
  def merge(that: GCounter): GCounter =
    val inner =
      (for
        k <- this.counters.keySet
      yield k -> math.max(this.counters(k), that.counters(k))).toMap
    GCounter(inner)
  end merge

  def total: Int = counters.values.sum
end GCounter

object GCounter:
  given gCounterShow
      : (Show[Map[String, Int]]) => Show[GCounter]:
    def s(t: GCounter): String = s"GCounter(${t.counters.show})"

@main def main =
  var a = GCounter(Map("A" -> 0, "B" -> 0))
  var b = GCounter(Map("A" -> 0, "B" -> 0))
  println(a.show)
  println(b.show)
  a = a.increment("A", 1)
  println(a.show)
  b = b.increment("B", 2)
  println(b.show)
  println(a.merge(b).show)
end main
