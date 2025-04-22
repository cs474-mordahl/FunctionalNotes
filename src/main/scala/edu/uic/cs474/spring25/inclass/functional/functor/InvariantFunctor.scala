package edu.uic.cs474.spring25.inclass.functional.functor

import scala.annotation.targetName

trait InvariantFunctor[F[_]]:
  def imap[A, B](init: F[A])(left: A => B, right: B => A): F[B]

object InvariantFunctor:
  extension [F[_]: InvariantFunctor, A](a: F[A])
    @targetName("imapSyntax")
    def imap[B](left: A => B, right: B => A): F[B] =
      summon[InvariantFunctor[F]].imap(a)(left, right)

  given InvariantFunctor[Codec]:
    def imap[A, B](init: Codec[A])(left: A => B, right: B => A): Codec[B] =
      new Codec[B]:
        def encode(a: B): String = init.encode(right(a))
        def decode(s: String): B = left(init.decode(s))
  end given
end InvariantFunctor

trait Codec[A]:
  def encode(a: A): String
  def decode(s: String): A
