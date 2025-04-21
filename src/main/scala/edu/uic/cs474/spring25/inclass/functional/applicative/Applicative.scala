package edu.uic.cs474.spring25.inclass.functional.applicative

import edu.uic.cs474.spring25.inclass.functional.functor.CovariantFunctor

/** Applicative laws:
  *  1. Identity: ap(pure(a => a))(v) == v
  *  2. Composition: ap(u)(ap(v)(w)) == ap(ap(ap(pure(compose))(u))(v))(w)
  *  3. Homomorphism: ap(pure(f))(pure(x)) == pure(f(x))
  *  4. Interchange: ap(u)(pure(y)) == ap(f => pure(f(y)))(u)
  */
trait Applicative[F[_]] extends CovariantFunctor[F]:
  def pure[A](a: A): F[A]
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
  def map2[A, B, C](fa: F[A], fb: F[B])(ff: (A, B) => C): F[C]

trait ApplicativeAlt[F[_]] extends CovariantFunctor[F]:
  def pure[A](a: A): F[A]
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
