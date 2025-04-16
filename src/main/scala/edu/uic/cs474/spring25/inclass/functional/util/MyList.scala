package edu.uic.cs474.spring25.inclass.functional.util

enum MyList[+T]:
  case NonEmptyList(h: T, tail: MyList[T])
  case EmptyList
  // Concatenate two lists together
end MyList

object MyList:
  def concat[T](l1: MyList[T], l2: MyList[T]): MyList[T] = ???
