// Define Boolean purely as codata
enum DataBoolean:
  case True
  case False

trait CodataBoolean:
  def `if`[T](t: T)(f: T): T

val True = new CodataBoolean:
  def `if`[T](t: T)(f: T): T = t

val False = new CodataBoolean:
  def `if`[T](t: T)(f: T): T = f

def And[T](b1: CodataBoolean, b2: CodataBoolean): CodataBoolean =
  new CodataBoolean:
    def `if`[T](t: T)(f: T): T =
      b1.`if`(b2)(False).`if`(t)(f)

def Or[T](b1: CodataBoolean, b2: CodataBoolean): CodataBoolean =
  new CodataBoolean:
    def `if`[T](t: T)(f: T): T =
      b1.`if`(True)(b2).`if`(t)(f)

True.`if`(true)(false) == true
False.`if`(true)(false) == false

// Truth table for And look like?
And(True, True).`if`("yes")("no")
And(True, False).`if`("yes")("no")
And(False, True).`if`("yes")("no")
And(False, False).`if`("yes")("no")

Or(And(Or(True, False), True), False).`if`("yes")("no")
Or(True, False).`if`("yes")("no")
Or(False, True).`if`("yes")("no")
Or(False, False).`if`("yes")("no")
