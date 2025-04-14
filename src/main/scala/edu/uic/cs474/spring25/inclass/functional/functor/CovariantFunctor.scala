package edu.uic.cs474.spring25.inclass.functional.functor

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
  val identity: List[Int] = List(1, 2, 3).map(a => a)

object CovariantFunctor:
  given CovariantFunctor[List]:
    def map[A, B](inst: List[A])(f: A => B): List[B] =
      inst.map(a => f(a))
