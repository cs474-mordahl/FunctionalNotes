package edu.uic.cs474.spring25.inclass.functional.data

import scala.annotation.tailrec

/** Algebraic Data Types are our primary tool for representing data in functional programming languages.
  *
  * They come in two forms:
  *    - SUM TYPES: A logical OR of types
  *    - PRODUCT TYPES: A logical AND of types
  */

/* Mechanism for product types is final case classes; note that if you look at
 * the bytecode, this implements the scala.Product interface. */
final case class C(x: Int, y: Int)

// Mechanism for sum types: two options

// First is a trait with case classes/objects that extend it.
trait Color
case object Red    extends Color
case object Blue   extends Color
case object Yellow extends Color

// Second is an enum (new in Scala 3).
enum Color2(name: String):
  case Red(name: "Red" = "Red")          extends Color2(name)
  case Blue(name: "Blue" = "Blue")       extends Color2(name)
  case Yellow(name: "Yellow" = "Yellow") extends Color2(name)

// => desugars to
// format: off
/*
sealed trait Color2(name: String)
final case class Red(name: "Red" = "Red")          extends Color2(name)
final case class Blue(name: "Blue" = "Blue")       extends Color2(name)
final case class Yellow(name: "Yellow" = "Yellow") extends Color2(name)
*/
// format: on

// ----- SET ------
enum Set[+T]:
  case EmptySet
  case Union(t: T, subset: Set[T])

  final def size(): Int =
    this match
      case EmptySet         => 0
      case Union(t, subset) => 1 + subset.size()

  @tailrec
  final def contains[U >: T](t: U): Boolean =
    this match
      case EmptySet         => false
      case Union(h, subset) => h == t || subset.contains(t)

  def take(n: Int): Set[T] =
    require(n <= this.size())
    this match
      case EmptySet         => EmptySet
      case Union(t, subset) =>
        if n == 0 then EmptySet
        else Union(t, subset.take(n - 1))
    end match
  end take
end Set

object Set:
  def apply[T](t: T)(rest: Set[T]): Set[T] = ???
