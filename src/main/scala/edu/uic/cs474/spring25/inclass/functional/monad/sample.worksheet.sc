import edu.uic.cs474.spring25.inclass.functional.monad.*

def initialValue = Some(4)

def timesByTwo(i: Int): Option[Int] =
  if i % 2 != 0 then None else Some(i * 2)
def divideByNine(i: Int): Option[Int] =
  if i > 100 then None else Some(i / 9)
def plusThree(i: Int): Option[Int] =
  if i < 5 then None else Some(i + 3)

// val x = Some(timesByTwo(i => Some(divideByNine(j => plusThree(Some(j))))))

def doAllThree(i: Int)(using m: Monad[Option]): Option[Int] =
  // timesByTwo(i).flatMap(e => divideByNine(e).flatMap(f => plusThree(f)))
  for
    e <- timesByTwo(i)
    g <- divideByNine(e)
    h <- plusThree(g)
    j <- timesByTwo(h)
  yield j

  /* timesByTwo(i).flatMap(e => divideByNine(e).flatMap(g => plusThree(g).map(h
   * => h + 1))) */

/* timesByTwo(i) match case None => None case Some(value) => divideByNine(value)
 * match case None => None case Some(value) => plusThree(value) match case None
 * => None case Some(value) => Some(value) */
