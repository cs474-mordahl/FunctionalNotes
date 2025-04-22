package edu.uic.cs474.spring25.inclass.functional
package functor

import util.MyList
import util.MyList.*
import scala.annotation.targetName

/** A covariant functor has two laws that it needs to follow:
  *    1. Identity (Identity function: a => a)
  *       for any k, k.map(a => a) == k
  *
  *    2. Composition
  *       For two functions f: A -> B, g: B -> C
  *       For a value of type F[A]
  *       value.map(f).map(g) == value.map(f . g)
  */

trait CovariantFunctor[F[_]]:
  def map[A, B](inst: F[A])(f: A => B): F[B]

object CovariantFunctor:
  extension [F[_]: CovariantFunctor, A](a: F[A])
    @targetName("mapSyntax")
    def map[B](f: A => B): F[B] =
      summon[CovariantFunctor[F]].map(a)(f)

  given CovariantFunctor[MyList]:
    def map[A, B](inst: MyList[A])(f: A => B): MyList[B] =
      inst match
        case NonEmptyList(h, tail) => NonEmptyList(f(h), tail.map(f))
        case EmptyList             => EmptyList
  end given
end CovariantFunctor
