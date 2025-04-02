import edu.uic.cs474.spring25.inclass.functional.monoid.*
import edu.uic.cs474.spring25.inclass.functional.monoid.Monoid.*
import edu.uic.cs474.spring25.inclass.functional.monoid.Monoid.given
import edu.uic.cs474.spring25.inclass.functional.semigroup.Semigroup
import edu.uic.cs474.spring25.inclass.functional.typeclasses.Ord

// Second implmentation: handle any map
final case class GCounter[K, V](counters: Map[K, V]):
  def increment(machine: K, amount: V)(using Semigroup[V]): GCounter[K, V] =
    GCounter(counters + (machine -> (counters(machine) |+| amount)))

  def merge(that: GCounter[K, V])(using Ord[V, V]): GCounter[K, V] =
    val inner =
      (for
        k <- this.counters.keySet
      yield
        /* Note that we no longer have access to math.max! We can write our own
         * max that utilizes Ord[A, B] */
        k -> this.counters(k).max(that.counters(k))).toMap
    GCounter(inner)
  end merge

  def total(using m: Monoid[V]): V =
    counters.values.foldLeft(m.identity)(_ |+| _)
end GCounter

// We can still use this with Map[String, Int]
var a = GCounter(Map("A" -> 0, "B" -> 0))
var b = GCounter(Map("A" -> 0, "B" -> 0))
a = a.increment("A", 1)
a
b = b.increment("B", 2)
b
a.merge(b)

// But we can also use this with Map[String, List[String]] (e.g., if we wanted to track IP addresses
var c: GCounter[String, List[String]] =
  GCounter(Map("A" -> List(), "B" -> List()))
var d: GCounter[String, List[String]] =
  GCounter(Map("A" -> List(), "B" -> List()))
c = c.increment("A", List("10.0.0.1"))
c
d = d.increment("B", List("192.168.0.6"))
d
c.merge(d)
