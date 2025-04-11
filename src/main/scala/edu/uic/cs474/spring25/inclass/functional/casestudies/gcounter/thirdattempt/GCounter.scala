package edu.uic.cs474.spring25.inclass.functional
package casestudies.gcounter.thridattempt

import typeclasses.show
import monoid.{BoundedSemilattice, CommutativeMonoid, Monoid}
import monoid.BoundedSemilattice.given
import monoid.CommutativeMonoid.given
import typeclasses.Show
import typeclasses.Show.*

/** For our third implementation, we will try to abstract away the fact that
  *  GCounter needs to be a Map. Instead, we will allow it to be any type with
  *  kind * -> * -> *, as long as we have a KeyValueStore defined for it.
  */
trait KeyValueStore[F[_, _]]:
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
  def get[K, V](f: F[K, V])(k: K): Option[V]
  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V
  def values[K, V](f: F[K, V]): List[V]
  def keySet[K, V](f: F[K, V]): Set[K]
end KeyValueStore

// Here, we provide the given instance for Maps.
given [K, V] => KeyValueStore[Map]:
  def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f ++ Map(k -> v)
  def get[K, V](f: Map[K, V])(k: K): Option[V]       = f.get(k)
  def getOrElse[K, V](f: Map[K, V])(k: K, default: V): V =
    f.getOrElse(k, default)
  def values[K, V](f: Map[K, V]): List[V] = f.values.toList
  def keySet[K, V](f: Map[K, V]): Set[K]  = f.keys.toSet
end given

/** And now, we define GCounter for any 2-arity type constructor.
  *
  * @tparam F The type representing our counter, which will be of type F[A, B]
  * @tparam A The "key" type of counters, usually representing a machine.
  * @tparam B The "value" type of our internal counter.
  * @param counters A
  */
final case class GCounter[F[_, _], A, B](counters: F[A, B]):
  def increment(machine: A, amount: B)(using
      cm: CommutativeMonoid[B],
      kv: KeyValueStore[F]
  ): GCounter[F, A, B] =
    val newCounter =
      kv.put(counters)(
        machine,
        kv.getOrElse(counters)(machine, cm.identity) |+| amount
      )
    GCounter(newCounter)
  end increment

  def merge(that: GCounter[F, A, B])(using
      kv: KeyValueStore[F]
  )(using
      BoundedSemilattice[F[A, B]],
      BoundedSemilattice[B]
  ): GCounter[F, A, B] =
    val keys = kv.keySet(this.counters) union kv.keySet(that.counters)
    def combineVals(k: A): B =
      (kv.get(this.counters)(k), kv.get(that.counters)(k)) match
        case (Some(v1), Some(v2)) => v1 |+| v2
        case (Some(v1), _)        => v1
        case (_, Some(v2))        => v2
        case _ => throw RuntimeException("This should never happen!")

    val vals = keys.map(k => k -> combineVals(k))
    var a    = this.counters
    for
      (k, v) <- vals
    do
      a = kv.put(a)(k, v)
    GCounter(a)
  end merge

  def total(using m: Monoid[B], kv: KeyValueStore[F]): B =
    kv.values(counters).foldLeft(m.identity)(_ |+| _)
end GCounter

object GCounter:
  given gcounterShow
      : [F[_, _], A, B] => (Show[F[A, B]]) => Show[GCounter[F, A, B]]:
    def s(t: GCounter[F, A, B]): String =
      s"GCounter(${t.counters.show})"
end GCounter

@main def main =
  /* This is the same example from secondattempt, showing that this definition
   * still allows us to use Maps as our counter type.
   *
   * Food for thought: are there any other types of the form F[_, _] that we
   * might want to use instead of Map? */
  def ex1 =
    println("Example 1: Map[String, Int]")
    var a = GCounter(Map("A" -> 0, "B" -> 0))
    var b = GCounter(Map("A" -> 0, "B" -> 0))
    println(a.show)
    println(b.show)
    a = a.increment("A", 1)(using intCommutativeMonoid)
    println(a.show)
    a = a.increment("A", 3)(using intCommutativeMonoid)
    println(a.show)
    b = b.increment("B", 2)
    println(b.show)
    println(a.merge(b).show)
  end ex1

  ex1
end main
