import scala.annotation.tailrec

/* For most things, we have the choice of representing it as data or codata.
 * However, some constructs (e.g., infinite data structures) can only be
 * represented as codata. */
trait Stream[+T]:
  def head: T
  def tail: Stream[T]
  def take(n: Int): List[T] =
    // Define as tail recursive so we can call it infinitely.
    @tailrec()
    def takeHelper(acc: List[T], n: Int, stream: Stream[T]): List[T] =
      if n == 0 then acc else takeHelper(acc :+ stream.head, n - 1, stream.tail)
    takeHelper(Nil, n, this)
  end take

  // Map transforms each value of the stream.
  def map[U](f: T => U): Stream[U] =
    val self = this
    new Stream[U]:
      def head: U         = f(self.head)
      def tail: Stream[U] = self.tail.map(f)
  end map
end Stream

// An infinite stream of ones.
val ones: Stream[Int] = new Stream[Int]:
  lazy val head =
    println("Getting the head")
    1
  lazy val tail =
    println("Getting the tail")
    ones

// The natural numbers
val naturals: Stream[Int] = new Stream[Int]:
  lazy val head: Int =
    println("Getting the value of head")
    0
  lazy val tail: Stream[Int] =
    println("Getting the value of tail")
    naturals.map((i: Int) => i + 10)

// An infinitely alternating stream of 1s and -1s
val alternating: Stream[Int] = new Stream[Int]:
  lazy val head = 1
  lazy val tail = head match
    case -1 => new Stream[Int]:
        def head: Int         = 1
        def tail: Stream[Int] = alternating
    case 1 => new Stream[Int]:
        def head: Int         = -1
        def tail: Stream[Int] = alternating

object Codata:
  def main(args: Array[String]): Unit =
    println(ones.take(20))
    println(naturals.take(20))
    println(alternating.take(20))
end Codata
