package dm

case class Course(
    year: Int,
    month: Int,
    code: Int,
    name: String,
    ects: Double) {

    override def equals(some: Any) = {
        some.isInstanceOf[Course] && some.asInstanceOf[Course].code == this.code
    }
    
    override def hashCode = (code).hashCode
}

object Course {
    def apply(ka: Array[String]) = {
        new Course(ka(0).split("-")(0).toInt, ka(0).split("-")(1).toInt, ka(1).toInt, ka(2), ka(3).toDouble)
    }

}
