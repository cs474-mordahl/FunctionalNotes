// Here, we will define a boolean purely as codata.
trait Boolean:
  def `if`[T](t: T)(f: T): T

val True = new Boolean:
  def `if`[T](t: T)(f: T): T = t

val False = new Boolean:
  def `if`[T](t: T)(f: T): T = f

// Now, we can define And and Or functions.
def And(left: Boolean, right: Boolean): Boolean = new Boolean:
  def `if`[T](t: T)(f: T): T = left.`if`(right)(False).`if`(t)(f)

def Or(left: Boolean, right: Boolean): Boolean = new Boolean:
  def `if`[T](t: T)(f: T): T = left.`if`(True)(right).`if`(t)(f)

// Truth table to verify
And(True, True).`if`("true")("false")
And(False, True).`if`("true")("false")
And(True, False).`if`("true")("false")
And(False, False).`if`("true")("false")

// Truth table to verify
Or(True, True).`if`("true")("false")
Or(True, False).`if`("true")("false")
Or(False, True).`if`("true")("false")
Or(False, False).`if`("true")("false")
