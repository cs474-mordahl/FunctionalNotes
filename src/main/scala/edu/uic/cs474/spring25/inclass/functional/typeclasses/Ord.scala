package edu.uic.cs474.spring25.inclass.functional.typeclasses

trait Ord[T, U]:
  def compare(t: T, u: U): Int
  extension (t: T)
    def max(u: U): T | U =
      if compare(t, u) > 0 then t else u
end Ord

object Ord:
  given Ord[Double, Double]:
    def compare(t: Double, u: Double): Int =
      if t < u then -1
      else if t == u then 0
      else 1
  end given

  given Ord[Int, Int]:
    def compare(t: Int, u: Int): Int =
      if t < u then -t
      else if t == u then 0
      else 1
  end given

end Ord
