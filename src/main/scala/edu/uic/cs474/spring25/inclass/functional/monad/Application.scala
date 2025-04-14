package edu.uic.cs474.spring25.inclass.functional
package monad

import Monad.*
import typeclasses.MyOption
import typeclasses.MyOption.*
import typeclasses.Show.*

def timesByTwo(i: Int): MyOption[Int] =
  println(s"timesByTwo got $i as input.")
  if i % 2 != 0 then MyNone else MySome(i * 2)
def divideByNine(i: Int): MyOption[Int] =
  println(s"divideByNine got $i as input.")
  if i < 100 then MyNone else MySome(i / 9)
def plusThree(i: Int): MyOption[Int] =
  println(s"plusThree got $i as input.")
  if i < 5 then MyNone else MySome(i + 3)

object Application:
  def doAll(i: Int)(using m: Monad[MyOption]): MyOption[Int] =
    // timesByTwo(i).flatMap(e => divideByNine(e).flatMap(f => plusThree(f)))
    for
      e <- timesByTwo(i)
      g <- divideByNine(e)
      h <- plusThree(g)
    yield h

  @main
  def main: Unit =
    val x = doAll(9000)
    println(x.show)
    val y = doAll(90)
    println(y.show)

    /* Note that only timesByTwo is called for this guy. That's because of the
     * short-circuiting behavior of Monads; in this case, the function is only
     * called if the value is a MySome. */
    val z = doAll(9)
    println(z.show)
  end main
end Application
