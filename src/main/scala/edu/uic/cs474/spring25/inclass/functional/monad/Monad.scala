package edu.uic.cs474.spring25.inclass.functional
package monad
import typeclasses.MyOption
import typeclasses.MyOption.*

/** A Monad provides sequencing behavior, allowing us to compose multiple
  * operations that work on a non-monadic context and returns a monadic context
  * (i.e., an "effect")
  *
  * A Monad provides two methods:
  *    - pure[A](a: A): F[A]
  *    - flatMap[A, B](init: F[A])(f: A => F[B]): F[A]
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

  given Monad[MyOption]:
    def pure[A](a: A): MyOption[A] = MySome(a)
    def _flatMap[A, B](init: MyOption[A])(f: A => MyOption[B]): MyOption[B] =
      init match
        case MyNone        => MyNone
        case MySome(value) => f(value)
  end given
end Monad
