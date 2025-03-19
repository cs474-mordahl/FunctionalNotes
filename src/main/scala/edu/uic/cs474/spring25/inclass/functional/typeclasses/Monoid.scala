package edu.uic.cs474.spring25.inclass.functional.typeclasses

trait Monoid[T] extends Semigroup[T]:
  def combine(t1: T)(t2: T): T
  def identity: T
  extension (t: T)
    def |+|(t2: T): T = combine(t)(t2)
end Monoid

object Monoid:
  def addition = new Monoid[Int]:
    def combine(t1: Int)(t2: Int): Int = t1 + t2
    def identity: Int                  = 0

  def multiplication = new Monoid[Int]:
    def combine(t1: Int)(t2: Int): Int = t1 * t2
    def identity: Int                  = 1

  def concat = new Monoid[String]:
    def combine(t1: String)(t2: String) = t1 + t2
    def identity: String                = ""

  def booleanAnd = new Monoid[Boolean]:
    def combine(t1: Boolean)(t2: Boolean): Boolean = t1 && t2
    def identity: Boolean                          = true

  def booleanOr = new Monoid[Boolean]:
    def combine(t1: Boolean)(t2: Boolean): Boolean = t1 || t2
    def identity: Boolean                          = false

  given [T: Monoid] => Monoid[Option[T]]:
    def combine(t1: Option[T])(t2: Option[T]): Option[T] =
      (t1, t2) match
        case (None, _)            => None
        case (_, None)            => None
        case (Some(s1), Some(s2)) => Some(s1 |+| s2)
    def identity: Option[T] = Some(summon[Monoid[T]].identity)
  end given
end Monoid
