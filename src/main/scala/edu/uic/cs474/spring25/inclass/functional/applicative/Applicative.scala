package edu.uic.cs474.spring25.inclass.functional
package applicative

import functor.*

trait Applicative1[F[_]] extends CovariantFunctor[F]:
  def pure[A](a: A): F[A]
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
  def product[A, B](fa: F[A])(fb: F[B]): F[(A, B)] =
    ap(ap(pure((a: A) => (b: B) => (a, b)))(fa))(fb)
end Applicative1

/** - Left Identity: pure(()).product(fa) == fa
  * - Right Identity: fa.product(pure()) == fa
  * - Associativity:
  */
trait Applicative2[F[_]] extends CovariantFunctor[F]:
  def pure[A](a: A): F[A]
  def product[A, B](fa: F[A])(fb: F[B]): F[(A, B)]
  def map2[A, B, C](fa: F[A], fb: F[B])(ff: (A, B) => C): F[C] =
    map(product(fa)(fb)):
      case (a, b) => ff(a, b)
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    map2(ff, fa)((f, a) => f(a))
end Applicative2
