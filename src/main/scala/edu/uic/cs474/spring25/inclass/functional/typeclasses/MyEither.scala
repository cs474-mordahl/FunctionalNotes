package edu.uic.cs474.spring25.inclass.functional.typeclasses

enum MyEither[A, B]:
  case Left(a: A)
  case Right(b: B)

object MyEither:
  def asLeft[A, B](a: A): MyEither[A, B]  = Left(a)
  def asRight[A, B](b: B): MyEither[A, B] = Right(b)
val x: MyEither[String, Int] = MyEither.asLeft[String, Int]("Hello")
