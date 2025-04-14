package edu.uic.cs474.spring25.inclass.functional.functor

import edu.uic.cs474.spring25.inclass.functional.typeclasses.Show

trait ContravariantFunctor[F[_]]:
  def contramap[A, B](init: F[A])(f: B => A): F[B]

object ContravariantFunctor:
  given ContravariantFunctor[Show]:
    def contramap[A, B](init: Show[A])(f: B => A): Show[B] = new Show[B]:
      def s(t: B): String = init.s(f(t))

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

@main
def main: Unit =
  given intShow: Show[Int]:
    def s(t: Int): String = t.toString()

  val doubleShow: Show[Double] =
    summon[ContravariantFunctor[Show]].contramap(intShow)(_.toInt)

  println(s"Printing a double: ${doubleShow.s(1.5)}")
end main
