package edu.uic.cs474.spring25.inclass.functional.applicative

trait Applicative1[F[_]]:
  def pure[A](a: A): F[A]
  def ap[A, B](fa: F[A], fb: F[B]): F[(A, B)]

/** - Left Identity: pure(()).product(fa) == fa
  * - Right Identity: fa.product(pure()) == fa
  * - Associativity:
  */
trait Applicative2[F[_]]:
  def pure[A](a: A): F[A]
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
