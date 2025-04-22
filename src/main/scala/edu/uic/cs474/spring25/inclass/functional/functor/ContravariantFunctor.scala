package edu.uic.cs474.spring25.inclass.functional.functor

import edu.uic.cs474.spring25.inclass.functional.typeclasses.Show

trait ContravariantFunctor[F[_]]:
  def _contramap[A, B](init: F[A])(f: B => A): F[B]

object ContravariantFunctor:
  extension [F[_]: ContravariantFunctor, A](a: F[A])
    def contramap[B](f: B => A): F[B] =
      summon[ContravariantFunctor[F]]._contramap(a)(f)

  given ContravariantFunctor[Show]:
    def _contramap[A, B](init: Show[A])(f: B => A): Show[B] = new Show[B]:
      def s(t: B): String = init.s(f(t))
end ContravariantFunctor

/* @main def main: Unit =
 * given doubleShow: Show[Double]:
 * def s(t: Double): String = t.toString()
 *
 * val intShow =
 * summon[ContravariantFunctor[Show]].contramap[ Double, Int ](doubleShow)(_ *
 * 1.0)
 *
 * println(s"I can show doubles: ${doubleShow.s(1.4)}") println(s"I can show
 * ints: ${intShow.s(6)}") end main */

import ContravariantFunctor.*

@main
def main: Unit =
  given intShow: Show[Int]:
    def s(t: Int): String = t.toString()

  val doubleShow: Show[Double] =
    intShow.contramap(_.toInt)

  println(s"Printing a double: ${doubleShow.s(1.5)}")
end main
