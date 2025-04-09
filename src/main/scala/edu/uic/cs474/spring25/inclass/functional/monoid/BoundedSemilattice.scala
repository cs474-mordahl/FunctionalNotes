package edu.uic.cs474.spring25.inclass.functional.monoid

trait BoundedSemilattice[T] extends CommutativeMonoid[T]

object BoundedSemilattice:
  given BoundedSemilattice[Int]:
    def combine(t1: Int, t2: Int): Int = math.max(t1, t2)
    def identity: Int                  = Int.MinValue

  given [T] => BoundedSemilattice[Set[T]]:
    def combine(t1: Set[T], t2: Set[T]): Set[T] = t1.union(t2)
    def identity: Set[T]                        = Set.empty[T]

  given mapSemilattice
      : [K, V: BoundedSemilattice] => BoundedSemilattice[Map[K, V]]:
    def combine(t1: Map[K, V], t2: Map[K, V]): Map[K, V] =
      def getBothVs(k: K): (K, V) =
        (t1.get(k), t2.get(k)) match
          case (Some(v1), Some(v2)) =>
            k -> (summon[BoundedSemilattice[V]].combine(v1, v2))
          case (Some(v1), _) => k -> v1
          case (_, Some(v2)) => k -> v2
          case (None, None) =>
            throw RuntimeException("This should never happen.")
      val allKeys = t1.keySet union t2.keySet
      allKeys.map(getBothVs(_)).toMap
    end combine

    def identity: Map[K, V] = Map()
  end mapSemilattice

end BoundedSemilattice
