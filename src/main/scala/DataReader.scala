package dm

import scala.io.Source
import scala.util.Sorting
import ArrayUtil._

object DataReader {
    def readCsv(dataPath: String): Array[Student] = {
        Source.fromFile( dataPath ).getLines.toArray.map(line => {
            val input = line.replace("\"","").split(" ")
            val aloitusvuosi = input(0).toInt
            val kurssistring = input.tail
            val kursseja = kurssistring.size / 5
            val suoritukset = new Array[Suoritus](kursseja)
            for (i <- 0 until kurssistring.size/5) {
                val ka = kurssistring.slice(i*5, (i+1)*5)
                suoritukset(i) = Suoritus(Course(ka.slice(0,4)), ka(4).toInt)
            }
            Student(aloitusvuosi,suoritukset)
        })
    }  

    def newCodes(data: Array[Student]) = {
        val codes = data.map(_.suoritukset.map(_.course.code)).flatten.distinct
        Sorting.quickSort(codes)
        codes.zipWithIndex.toMap
    }
    
    def setNewCodes(data: Array[Student]) = {
        val newcodes = newCodes(data)
        data.map(student => new Student(student.year, student.suoritukset.map(suoritus => new Suoritus(new Course(suoritus.course.year, suoritus.course.month, newcodes(suoritus.course.code), suoritus.course.name, suoritus.course.ects), suoritus.grade))))
    }

    def readInt = {
        val data = readCsv("data.csv")
            .map(student => 
                student.suoritukset
                    .map(suoritukset => 
                        suoritukset.course.code)
            )
        data
    }
    
    def negativeCourses(data: Array[Student]) = {
        val courseCodes = data.map(_.suoritukset.map(_.course.code)).flatten.distinct 
        val newSet = data.map(student => courseCodes - student.suoritukset.map(_.course.code))
        newSet.foreach(Sorting.quickSort(_))
        newSet
    }

    /**
    * Reads an array of course codes from file.
    * @param dataPath filepath to a file where there is one course code per line
    * @return Array[Int] of course codes
    */
    def getCourses(dataPath: String): Array[Int] = {
        Source.fromFile(dataPath).getLines.toArray.map(_.toInt) 
    }
}
