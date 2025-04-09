package edu.uic.cs474.spring25.inclass.functional.casestudies.gcounter.thridattempt

import edu.uic.cs474.spring25.inclass.functional.monoid.CommutativeMonoid
import edu.uic.cs474.spring25.inclass.functional.monoid.BoundedSemilattice

trait KeyValueStore[F[_, _]]:
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
  def get[K, V](f: F[K, V])(k: K): Option[V]
  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V
  def values[K, V](f: F[K, V]): List[V]
  def keySet[K, V](f: F[K, V]): Set[K]
  def construct[K, V]: F[K, V]
end KeyValueStore

final case class GCounter[F[_, _], A, B](counters: F[A, B]):
  def increment(machine: A, amount: B)(using
      CommutativeMonoid[B],
      KeyValueStore[F]
  ): GCounter[F, A, B] =
    GCounter(???)

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
    var a    = kv.construct[A, B]
    for
      (k, v) <- vals
    do
      a = kv.put(a)(k, v)
    GCounter(a)
  end merge

  def total(using m: BoundedSemilattice[B], kv: KeyValueStore[F]): B =
    kv.values(counters).foldLeft(m.identity)(_ |+| _)
end GCounter
