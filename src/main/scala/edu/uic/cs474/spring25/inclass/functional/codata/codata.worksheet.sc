/** An alternate definition of a type can be given via *codata*.
  * As opposed to data, where a type is defined by the data it holds,
  * Codata is defined via the functions it has.
  *
  * We usually use traits to provide a codata definition.
  */

/* There are multiple ways to implement codata, which often overlap.
 * When we have methods that return an instance of our codata, it is often best
 * to have them return an anonymous subtype of our codata. */
trait Set[+T]:
  def contains[U >: T](u: U): Boolean
  def size(): Int

  // Both remove and add return an instance of a new anonymous subclass
  //  of Set.
  def remove[U >: T](takeElem: U): Set[U] =
    val parent = this
    new Set[U]:
      override def contains[U >: T](u: U): Boolean =
        if takeElem == u then false else parent.contains(u)
      override def size(): Int                     =
        if parent.contains(takeElem) then parent.size() - 1 else parent.size()
    end new
  end remove

  def add[U >: T](newElem: U): Set[U] =
    val parent = this
    new Set[U]:
      override def contains[U >: T](u: U): Boolean =
        u == newElem || parent.contains(u)
      override def size(): Int                     =
        if !parent.contains(newElem) then parent.size() + 1 else parent.size()
    end new
  end add
end Set

// For the methods that don't return our codata, we can implement them
//  in final case classes.
final case class ListSet[+T](elems: List[T]) extends Set[T]:
  def contains[U >: T](u: U): Boolean =
    elems.contains(u)
  def size(): Int                     = elems.size
end ListSet

val x = ListSet(List(1, 2, 3))
x.size()

// Add an element.
val y = x.add(4)
y.size()
y.contains(4)
y.contains(-1)

// Remove an element that doesn't exist.
val z = y.remove(9)
z.size()
z.contains(9)
z.contains(3)

// Remove an element that does exist.
val a = z.remove(1)
a.size()
a.contains(1)
a.contains(2)
