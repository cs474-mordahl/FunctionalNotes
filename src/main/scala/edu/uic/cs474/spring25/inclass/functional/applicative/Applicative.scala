package edu.uic.cs474.spring25.inclass.functional
package applicative

import functor.CovariantFunctor
import functor.CovariantFunctor.*
import scala.annotation.targetName

/** Applicative functors are an intermediate structure between functors and
  * monads. Whereas monads provide sequencing behavior (i.e., running multiple
  * operations of type A => F[A] where one computation relies on the previous),
  * applicatives allow us to combine effectful values in an independent way.
  *
  * In this file, we will go over two different equivalent formulations of
  * Applicatives; using ap, and using product. These are equivalent, because ap
  * can be defined in terms of pure, map (from Functor), and ap, whereas ap can
  * be defined in terms of map, ap, pure (and usually a method called map2
  * which we can derive from the extant methods.)
  *
  * Applicative laws:
  *  1. Identity: ap(pure(a => a))(v) == v
  *  2. Composition: ap(u)(ap(v)(w)) == ap(ap(ap(pure(compose))(u))(v))(w)
  *  3. Homomorphism: ap(pure(f))(pure(x)) == pure(f(x))
  *  4. Interchange: ap(u)(pure(y)) == ap(f => pure(f(y)))(u)
  */

/** This is the first form of Applicative, where `pure` and `ap` are left
  * abstract and other methods are defined in terms of them.
  */
trait Applicative[F[_]] extends CovariantFunctor[F]:
  def pure[A](a: A): F[A]

  def ap[A, B](fa: F[A])(ff: F[A => B]): F[B]

  // Product implemented in terms of ap and pure
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    ap(fb):
      ap(fa):
        pure: (a: A) =>
          (b: B) => (a, b)

  // Map (from functor) can be implemented in terms of ap and pure
  def map[A, B](inst: F[A])(f: A => B): F[B] = ap(inst)(pure(f))

  // Map2, and its generalization to mapN, allows us to take functions that
  //  accept multiple arguments and apply them to multiple effectful parameters.
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    map(product(fa, fb)): x =>
      x match
        case (_1, _2) => f(_1, _2)

  // Use the same pattern that we used to implement map2. */
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    def product3(fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] =
      ap(fc):
        ap(fb):
          ap(fa):
            pure: (a: A) =>
              (b: B) => (c: C) => (a, b, c)
    map(product3(fa, fb, fc)): (a, b, c) =>
      f(a, b, c)
  end map3
end Applicative

enum MyValidated[+I, +V]:
  case Valid(v: V)
  case Invalid(i: I)

object Applicative:
  given listValidatedApplicative
      : [V] => Applicative[[V] =>> MyValidated[List[String], V]]:
    def ap[A, B](fa: MyValidated[List[String], A])(ff: MyValidated[
      List[String],
      A => B
    ]): MyValidated[List[String], B] =
      import MyValidated.*
      (fa, ff) match
        case (Valid(a), Valid(f))     => Valid(f(a))
        case (Invalid(a), Valid(f))   => Invalid(a)
        case (Valid(a), Invalid(f))   => Invalid(f)
        case (Invalid(a), Invalid(f)) => Invalid(a ::: f)
      end match
    end ap

    def pure[A](a: A): MyValidated[List[String], A] = ???
  end listValidatedApplicative

  extension [F[_]: Applicative, A](a: F[A])
    @targetName("apSyntax")
    def ap[B](f: F[A => B]): F[B] =
      summon[Applicative[F]].ap(a)(f)
    @targetName("map2Syntax")
    def map2[B, C](fb: F[B])(ff: (A, B) => C): F[C] =
      summon[Applicative[F]].map2(a, fb)(ff)
    @targetName("productSyntax")
    def product[B](fb: F[B]): F[(A, B)] =
      summon[Applicative[F]].product(a, fb)
  end extension
end Applicative

trait ApplicativeAlt[F[_]] extends CovariantFunctor[F]:
  def pure[A](a: A): F[A]
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  // ap defined in terms of map and product; proof that both formulations of
  // applicative are equivalent
  def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] =
    map(product(fa, ff))(tu => tu._2(tu._1))
  def map[A, B](inst: F[A])(f: A => B): F[B] = ap(inst)(pure(f))
end ApplicativeAlt
