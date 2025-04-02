package edu.uic.cs474.spring25.inclass

trait Show[T]:
  def _show(t: T): String
  extension (t: T)
    def show = _show(t)
