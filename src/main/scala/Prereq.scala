package dm
import scala.util.Sorting
    
object Prerequisites {

    implicit class SuperInt(i: Int) {
        def a(x: Int) = {
            (i.toString + x.toString).toInt
        }
    }
    def rulesForPassing(data: Array[Student], coursecode: Int, minconf: Double, maxrulesize: Int) = {
        val filtered = data
            .filter(_.hasCompletedOrAttempted(coursecode))
            .map(_.suoritukset.map(s => {
                val oc = s.course
                val newcode = {if (s.grade == 0) s.course.code.a(0) else s.course.code.a(1)}
                new Course(oc.year, oc.month, newcode, oc.name, oc.ects)
            }))
        
        val filteredintime = filtered.map( coursearray => {val fa = firstAttemptToTake(coursecode, coursearray); coursearray.filter(x => x.year <= fa._1 && x.month <= fa._2)})

        val gooddata = FreqUtils.suitabledata(filteredintime)
        val frequentcourses = gooddata.flatten.distinct.map(x => Array(x)).filter(FreqUtils.support(gooddata, _) > 0.05)
        FreqUtils.genrulesForConsequent(minconf, 0, maxrulesize, gooddata, Array(coursecode.a(1)), frequentcourses)
    }

    def firstAttemptToTake(code: Int, courses: Array[Course]): (Int, Int) = {
            val code1 = (code + "0").toInt
            val code2 = (code + "1").toInt
            val firstTime = courses.filter(x => x.code == code1 || x.code == code2).map(course => course.year + course.month.toDouble/100).min
            (firstTime.toInt, (firstTime*100).toInt - 100*firstTime.toInt) 
    }
}
