import scala.annotation.tailrec

/** In functional programming, we have two primary representations for
  *  representing "data": these are "data" and "codata". This worksheet deals
  *  with data, while the other worksheet deals with codata.
  */

/** Algebraic Data Types are our primary tool for representing data in
  * functional programming languages.
  *
  * They come in two forms:
  *    - SUM TYPES: A logical OR of types
  *    - PRODUCT TYPES: A logical AND of types
  *
  * Critically, both sum and product types can be composed of other sum or
  * product types.
  */

// ----- PRODUCT TYPES -----

/** A product type represents a logical AND. In practice, these are types that
  * have data constructors that take multiple parameters.
  *
  * For example, consider the following case class:
  */
final case class C(x: Int, y: Int)

/** This type is a product of two ints. The reason these are called product
  * types is that the cardinality is a product of the cardinalities of the
  * constituent types. So, there are |Int| * |Int| possible values for C.
  *
  * Indeed, (final) case classes are the canonical mechanism for implementing
  * product types in Scala. Actually, if you look at the bytecode for C, you'll
  * see that case classes all implement the scala.Product trait, which we can
  * utilize to automatically generate typeclass instances.
  */

// ----- SUM TYPES -----

// There are two options for sum types:

// First is an enum (new in Scala 3).
enum Color1(name: String):
  case Red1(name: "Red" = "Red")          extends Color1(name)
  case Blue1(name: "Blue" = "Blue")       extends Color1(name)
  case Yellow1(name: "Yellow" = "Yellow") extends Color1(name)

  def getName(): String = this.name
end Color1

// Enums are nice, but they (currently) have limitations; e.g., we cannot
//  have nested enums. We can define methods inside the enum, which will be
//  inherited by all cases (see getName() above).

import Color1.*
Red1().getName()
Blue1().getName()

// Second is a trait with case classes/objects that extend it.
sealed trait Color2:
  def name: String

case object Red2    extends Color2:
  def name: "Red" = "Red"
case object Blue2   extends Color2:
  def name: "Blue" = "Blue"
case object Yellow2 extends Color2:
  def name: "Yellow"            = "Yellow"
  def onlyDefinedForYellow(): 1 = 1

// This is the more flexible implementation, and allows us to have pseudo-ADTs,
//  where methods are defined on only certain instances (e.g., see
// `onlyDefinedForYellow` above.)
Yellow2.onlyDefinedForYellow()

// ----- DATA vs. CODATA -----
// Data is the "traditional" definition of types; the value is defined
//  by the things that it *has*. This shows a data definition of a Set.
enum Set[+T]:
  // Note that our two instances are differentiated based on what they contain.
  case EmptySet
  case Union(t: T, subset: Set[T])

  // We can now define methods on Set. To define functions, we follow two
  //  primary strategies:

  // To define functions that consume ADTs, we use *structural recursion.*
  //  Structural recursion is a strategy in which we break down the input
  //  into its cases (typically using pattern matching), and handle each
  //  type separately.

  // This implicitly takes a set:
  final def size(): Int =
    // Per structural recursion, we pattern match, breaking down the structure:
    this match
      // base case
      case EmptySet         => 0
      // recursive case: whenever we handle a recursive case, we need a
      //  recursive call.
      case Union(t, subset) => 1 + subset.size()

  // We can also do this using structural recursion.
  // Notably, this is a tail-recursive call, where
  //  the recursive call is in tail position.
  // Therefore, we can tell the compiler that this is a tail recursive call
  //  and it will optimize it for us.
  @tailrec
  final def contains[U >: T](t: U): Boolean =
    this match
      case EmptySet         => false
      case Union(h, subset) => h == t || subset.contains(t)

  // This takes a set and returns a set. Therefore, we can use both
  //  structural recursion and structural corecursion.
  def take(n: Int): Set[T] =
    require(n <= this.size())
    // Structural recursion
    this match
      case EmptySet         => EmptySet
      case Union(t, subset) =>
        // Structural corecursion
        if n == 0 then EmptySet
        else Union(t, subset.take(n - 1))
    end match
  end take
end Set

object Set:
  // We use structural corecursion here, where we use an if-then-else construct
  //  to produce each potential element.
  def apply[T](items: Seq[T]): Set[T] =
    // Structural corecursion template:
    // if ??? then EmptySet else Union(???, ???)

    // Anywhere we have a ??? that refers to a field of our ADT type,
    //  we replace it with a recursive call.
    if items.isEmpty then EmptySet
    // ^^^^^^^^^^^^^ condition
    else Union(items.head, Set.apply(items.tail))
    //         ^^^^^^^^^^  ^^^^^^^^^^^^^^^ recursive
    //         ^ non-recursive
end Set

val x = Set(List(1, 2, 3))
x
x.contains(1)
x.contains(4)

// Data is easy to add functions to, but it is hard to add new cases to data.
