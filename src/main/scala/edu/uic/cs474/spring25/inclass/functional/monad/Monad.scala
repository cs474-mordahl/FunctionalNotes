package edu.uic.cs474.spring25.inclass.functional
package monad

import util.MyList.*
import util.MyOption.*
import util.{MyEither, MyList, MyOption}

import util.MyEither.*

/** A Monad provides sequencing behavior, allowing us to compose multiple
  * operations that work on a non-monadic context and returns a monadic context
  * (i.e., an "effect")
  *
  * A Monad provides two methods:
  *    - pure[A](a: A): F[A]
  *    - flatMap[A, B](init: F[A])(f: A => F[B]): F[A]
  *
  * A Monad has to follow three laws:
  *    - left identity: pure(a).flatMap(f) = f(a)
  *    - right identity: x.flatMap(x => pure(x)) = x
  *    - associativity:
  */
trait Monad[F[_]]:
  def pure[A](a: A): F[A]
  def _flatMap[A, B](init: F[A])(f: A => F[B]): F[B]
  def _map[A, B](init: F[A])(f: A => B): F[B] =
    _flatMap(init)(i => pure(f(i)))
end Monad

object Monad:
  extension [A, F[_]: Monad](f: F[A])
    def flatMap[B](g: A => F[B]): F[B] = summon[Monad[F]]._flatMap(f)(g)
    def map[B](g: A => B): F[B]        = summon[Monad[F]]._map(f)(g)

  given myOptionMonad: Monad[MyOption]:
    def pure[A](a: A): MyOption[A] = MySome(a)
    def _flatMap[A, B](init: MyOption[A])(f: A => MyOption[B]): MyOption[B] =
      init match
        case MyNone        => MyNone
        case MySome(value) => f(value)
  end myOptionMonad

  given myListMonad: Monad[MyList]:
    def pure[A](a: A): MyList[A] = NonEmptyList(a, MyList.EmptyList)
    def _flatMap[A, B](init: MyList[A])(f: A => MyList[B]): MyList[B] =
      init match
        case NonEmptyList(h, tail) => MyList.concat(f(h), tail.flatMap(f))
        case EmptyList             => EmptyList
  end myListMonad

  // Note that in class, I wrote this by introducing a new type,
  // type myRightEither[B] = MyEither[String, B].
  // Here, I have changed it to use a type lambda, which is equivalent.
  given myEitherStringMonad: [B] => Monad[[B] =>> MyEither[String, B]]:
    def pure[A](a: A): MyEither[String, A] = a.asRight[String]
    def _flatMap[A, B](init: MyEither[String, A])(f: A => MyEither[String, B])
        : MyEither[String, B] =
      init match
        case Right(a: A)     => f(a)
        case Left(s: String) => s.asLeft[B]
  end myEitherStringMonad

end Monad
