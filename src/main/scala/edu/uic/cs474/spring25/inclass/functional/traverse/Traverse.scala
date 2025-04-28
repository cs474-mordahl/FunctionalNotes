package edu.uic.cs474.spring25.inclass.functional
package traverse

import applicative.Applicative

/** Traverse is a very high-level typeclass that abstracts away the operation
  * of "traversing" over an effectful value.
  */
trait Traverse[F[_]]:
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

enum MyTree[+T]:
  case Branch(v: T, l: MyTree[T], r: MyTree[T])
  case Leaf
object Traverse:
  given Traverse[MyTree]:
    def traverse[G[_]: Applicative, A, B](fa: MyTree[A])(f: A => G[B])
        : G[MyTree[B]] = fa match
      case MyTree.Branch(v, l, r) => summon[Applicative[G]].map3(
          f(v),
          traverse(l)(f),
          traverse(r)(f)
        )(MyTree.Branch(_, _, _))
      case MyTree.Leaf => summon[Applicative[G]].pure(MyTree.Leaf)
  end given
end Traverse

given Applicative[List]:
  def ap[A, B](fa: List[A])(ff: List[A => B]): List[B] =
    summon[cats.Applicative[List]].ap(ff)(fa)
  def pure[A](a: A): List[A] = List(a)
