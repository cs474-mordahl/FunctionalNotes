package edu.uic.cs474.spring25.inclass.functional.typeclasses

enum MyOption[+T]:
  case MySome(t: T)
  case MyNone
