package edu.uic.cs474.spring25.inclass.functional.algebraicdatatypes.codata

class MySet[T]:
  def contains(t: T): Boolean = false
  def size(): Int             = 0

  def remove(takeElem: T): MySet[T] = new MySet[T]:
    override def contains(t: T): Boolean =
      if t == takeElem then false else super.contains(t)

    override def size(): Int = super.size() - 1

  def add(newElem: T): MySet[T] = new MySet[T]:
    override def contains(t: T): Boolean =
      newElem == t || super.contains(t)

    override def size(): Int = 1 + super.size()
end MySet

object Main:
  def main(args: Array[String]): Unit =
    val x = MySet[Int]()
    val y = x.add(1)
    val z = y.add(2)
    print(z.contains(3))
  end main
end Main
