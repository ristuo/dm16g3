package dm
import scala.util.Sorting
    
object Prerequisites {
    //for giggles i try with measure theory
    val mathcourses = DataReader.getCourses("mathcourses.csv")
    val mathcoursenames = data.map(_.suoritukset.map(_.course)).flatten.distinct.filter(course => mathcourses.contains(course.code)).map(x => (x.code,x.name)).toMap 
    val data = DataReader.readCsv("data.csv")
    val allcoursecodes = data.map(_.suoritukset.map(_.course.code)).flatten.distinct
    
    val measuredata = data.filter(_.suoritukset
            .filter(_.grade == 0)
            .map(_.course.code).contains(57101)
        )
    val negativemeasuredata = DataReader.negativeCourses(measuredata).map(_.filter(code => mathcourses.contains(code)))
    val res = FreqUtils.apriori(negativemeasuredata, 0.8, 10)
    res.map(_.map(code => mathCourseNames(code)))

}
