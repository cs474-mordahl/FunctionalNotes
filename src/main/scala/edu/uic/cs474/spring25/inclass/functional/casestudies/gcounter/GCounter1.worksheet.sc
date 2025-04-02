import edu.uic.cs474.spring25.inclass.functional.monoid.*
import edu.uic.cs474.spring25.inclass.functional.monoid.Monoid.given
import edu.uic.cs474.spring25.inclass.functional.semigroup.Semigroup

import math.*

trait KeyValueStore[F[_, _]]:
  def get[K, V](f: F[K, V])(k: K): Option[V]
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V
  def values[K, V](f: F[K, V]): Iterable[V]
  def keys[K, V](f: F[K, V]): Iterable[K]
end KeyValueStore

given KeyValueStore[Map] with
  def get[K, V](f: Map[K, V])(k: K): Option[V]       = f.get(k)
  def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)
  def getOrElse[K, V](f: Map[K, V])(k: K, default: V): V =
    f.getOrElse(k, default)
  def values[K, V](f: Map[K, V]): Iterable[V] = f.values
  def keys[K, V](f: Map[K, V]): Iterable[K]   = f.keys
end given

// --- FIRST IMPLEMENTATION: handle a specific counter structure ---
final case class GCounter[F[_, _], K, V](counters: F[K, V]):
  def increment(machine: K, amount: V)(using
      m: Monoid[V],
      f: KeyValueStore[F]
  ): GCounter[F, K, V] =
    GCounter(f.put(counters)(
      machine,
      f.getOrElse(counters)(machine, m.identity) |+| amount
    ))

  def merge(that: GCounter[F, K, V])(using
      b: BoundedSemilattice[V],
      f: KeyValueStore[F]
  ): GCounter[F, K, V] =
    val inner: Iterable[(K, V)] =
      for
        k <- f.keys(this.counters)
      yield k -> (f.getOrElse(counters)(
        k,
        b.identity
      ) |+| f.getOrElse(that.counters)(k, b.identity))

    var res: F[K, V] = this.counters
    for
      (k, v) <- inner
    do
      res = f.put(res)(k, v)

    GCounter(res)
  end merge

  def total(using m: Monoid[V], f: KeyValueStore[F]): V =
    f.values(counters).fold(m.identity)(_ |+| _)
end GCounter

var a: GCounter[Map, String, List[String]] =
  GCounter(Map("A" -> List("192.168.0.1"), "B" -> List()))
var b: GCounter[Map, String, List[String]] =
  GCounter(Map("A" -> List(), "B" -> List()))
a = a.increment("A", List("10.0.0.1"))
a
b = b.increment("B", List("192.168.0.1"))
b
a.merge(b)
