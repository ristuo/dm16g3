package dm

object ArrayUtil {
    implicit class CoolArray(a: Array[Int]) {
        def -(x: Array[Int]) = a.filter(!x.contains(_))
        def ts() = if (a.size==0) "" else "" + a(0) + a.tail.foldLeft("")( (a,b) => a + "," + b)
    }
}
