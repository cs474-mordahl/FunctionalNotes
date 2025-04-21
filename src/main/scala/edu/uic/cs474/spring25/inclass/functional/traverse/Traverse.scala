package edu.uic.cs474.spring25.inclass.functional
package traverse

import applicative.Applicative

def traverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] =
  as.foldRight(summon[Applicative[F]].pure(List.empty[B])):
    (a: A, acc: F[List[B]]) =>
      val fb: F[B] = f(a)
      summon[Applicative[F]].map2(fb, acc)(_ :: _)
