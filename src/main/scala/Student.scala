package dm

case class Student(year: Int, suoritukset: Array[Suoritus]) {
    override def toString() = {
        var tulos = "Aloitusvuosi: " + this.year + "\n"
        suoritukset.foreach(s => {
            tulos = tulos + "\t" + s.course.name + " " + s.grade + "\n"
        })
        tulos
    }

    def credits() = {
        this.suoritukset.map(_.course).distinct
            .map(_.ects).sum
    }
    
    def hasDoneWithGrade(course: Int, grade: Int) = {
        this.suoritukset.filter(suoritus => suoritus.course.code == course && suoritus.grade == grade).size >= 1
    }

    def average = this.suoritukset.map(_.grade.toDouble).reduce(_+_)/suoritukset.size

    def hasCompletedOrAttempted(courseName: String = "Kandidaatintutkielma") = {
        this.suoritukset.map(_.course.name).map(_ == courseName).foldLeft(false)( (a,b) => a || b)
    }

    def hasCompleted(courseName: String = "Kandidaatin_tutkielma") = {
        this.suoritukset.filter(_.grade > 0).map(_.course.name).map(_ == courseName).foldLeft(false)( (a,b) => a || b)
    }

    def hasDoneBsc() = {
        this.hasCompleted() && this.credits >= 180
    }

    def hasCompletedOrAttempted(courseCode: Int) = {
        this.suoritukset.map(_.course.code).map(_ == courseCode).foldLeft(false)( (a,b) => a || b)
    }

   def isMastersStudent() = {
        !( (this.hasCompletedOrAttempted("Johdatus_yliopistomatematiikkaan")) || this.hasCompletedOrAttempted(57049))
    } 

    def hasAttemptedAnyCourseLately() = {
        this.suoritukset.map(suoritus => suoritus.course.year > 2013 && suoritus.course.month > 5).foldLeft(false)( (a,b) => a || b)
    }
}
