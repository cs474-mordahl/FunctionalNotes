package edu.uic.cs474.spring25.inclass.functional.util

enum MyEither[+L, +R]:
  case Left(l: L)
  case Right(r: R)

// Provide utility methods to construct left and right instances.
object MyEither:
  extension [T](t: T)
    def asLeft[R]: MyEither[T, R]  = Left[T, R](t)
    def asRight[L]: MyEither[L, T] = Right[L, T](t)
end MyEither
