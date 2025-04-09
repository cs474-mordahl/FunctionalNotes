import edu.uic.cs474.spring25.inclass.functional.casestudies.gcounter.secondattempt.GCounter
import edu.uic.cs474.spring25.inclass.functional.monoid.*
import edu.uic.cs474.spring25.inclass.functional.monoid.BoundedSemilattice.given

// We can still use this with Map[String, Int]
var a = GCounter(Map("A" -> 0, "B" -> 0))
var b = GCounter(Map("A" -> 0, "B" -> 0))
a = a.increment("A", 1)
a
b = b.increment("B", 2)
b
a.merge(b)

// But we can also use this with Map[String, List[String]] (e.g., if we wanted to track IP addresses
var c: GCounter[String, Set[String]] =
  GCounter(Map("A" -> Set(), "B" -> Set()))
var d: GCounter[String, Set[String]] =
  GCounter(Map("A" -> Set(), "B" -> Set()))
c = c.increment("A", Set("10.0.0.1"))
c
d = d.increment("B", Set("192.168.0.6", "192.168.0.7"))
d
c.merge(d)
