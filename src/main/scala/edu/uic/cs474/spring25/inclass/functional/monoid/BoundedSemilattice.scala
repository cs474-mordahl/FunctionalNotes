package edu.uic.cs474.spring25.inclass.functional.monoid

trait BoundedSemilattice[T] extends CommutativeMonoid[T]

object BoundedSemilattice:
  given BoundedSemilattice[Int] with
    def combine(t1: Int, t2: Int): Int = math.max(t1, t2)
    def identity                       = Int.MinValue

  given BoundedSemilattice[Double] with
    def combine(t1: Double, t2: Double): Double = math.max(t1, t2)
    def identity                                = Double.MinValue

  given [T: Ordering]: BoundedSemilattice[List[T]] with
    def combine(t1: List[T], t2: List[T]): List[T] =
      t1.concat(t2).sorted

    def identity = Nil
  end given
end BoundedSemilattice
