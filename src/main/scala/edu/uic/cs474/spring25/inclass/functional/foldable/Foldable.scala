package edu.uic.cs474.spring25.inclass.functional.foldable
import cats.Eval
import scala.annotation.tailrec
import scala.annotation.meta.param

/** An instance of Foldable for F indicates that we can perform
  * foldLeft and foldRight on F.
  */
trait Foldable[F[_]]:
  def foldLeft[A, B](acc: B)(li: F[A])(f: (B, A) => B): B
  def foldRight[A, B](acc: B)(li: F[A])(f: (A, Eval[B]) => Eval[B]): Eval[B]
end Foldable

object Foldable:
  given Foldable[List]:

    @tailrec
    def foldLeft[A, B](acc: B)(li: List[A])(f: (B, A) => B): B = li match
      case head :: next => foldLeft(f(acc, head))(next)(f)
      case Nil          => acc

    def foldRight[A, B](acc: B)(li: List[A])(f: (
        A,
        Eval[B]
    ) => Eval[B]): Eval[B] = li match
      case head :: next => Eval.defer(f(head, foldRight(acc)(next)(f)))
      case Nil          => Eval.now(acc)
  end given
end Foldable

@main
def main =
  val x = List.fill(100000000)(false)
  println(summon[Foldable[List]].foldRight(true)(x)((i, e) =>
    e.map(_ && i)
  ).value)
end main
