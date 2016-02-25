package dm

import scala.util.control._
import scala.util.Sorting

object FreqUtils {
    
    def suitabledata(data: Array[Student]) = {
        val tmp = data.map(student => student.suoritukset.map(suoritus => suoritus.course.code).distinct)
        tmp.foreach(x => Sorting.quickSort(x))
        tmp
    }

    def droppy(x: Array[Int], k: Int) = (x.view.take(k) ++ x.view.drop(k+1)).toArray

    //wow scala is so great! with this one simple trick i can actually write Array[Int] - Array[Int] for difference of sets
    implicit class CoolArray(a: Array[Int]) {
        def -(x: Array[Int]) = a.filter(!x.contains(_))
        def ts() = if (a.size==0) "" else "" + a(0) + a.tail.foldLeft("")( (a,b) => a + "," + b)
    }


    def support( data: Array[Array[Int]], codes: Array[Int])(implicit d: DummyImplicit) = {
        countSet(data, codes).toDouble/data.size.toDouble
    }

    def countSet( data: Array[Array[Int]], codes: Array[Int]) = {
        data.filter(courses => codes.forall(code => courses.contains(code))).size
    }

    case class Rule(val premiss: Array[Int], consequent: Array[Int], conf: Double, lift: Double) {
        override def toString() = premiss.ts + " ---> " + consequent.ts + " " + conf
    }

    def conf(x: Rule, data: Array[Array[Int]]) = support(data, x.premiss ++ x.consequent)/support(data, x.premiss)

    def lift(x: Rule, data: Array[Array[Int]]) = conf(x, data)/support(data,x.consequent)

    def is(x: Rule, data: Array[Array[Int]]) = Math.sqrt( lift(x,data)*support(data,x.premiss++x.consequent) )

    def apgenrules(depth: Int, fk: Array[Int], hm: Array[Array[Int]], data: Array[Array[Int]], minconf: Double, minlift: Double): List[Rule] = {
        if (depth > 10)
            return List[Rule]()
        if (hm.size == 0)
            return List[Rule]()
        val k = fk.size
        val m = hm(0).size
        var res = List[Rule]()
        if (k > m + 1) {
            val hm1 = apriorigen(hm)
            val hm1updated = hm1.map(consequent => {
                //look how nicely the implicit class works here! 
                val conf = support(data, fk)/support(data, fk - consequent)
                val lift = conf/support(data,consequent)
                if (conf > minconf && lift > minlift) {
                    //println(s"${(fk - consequent).ts} ${consequent.ts} $conf")
                    res = res ++ List(Rule(fk - consequent, consequent, conf, lift))
                    consequent
                } else {
                    new Array[Int](0)                    
                }
            }).filter(_.size > 0)
        return res ++ apgenrules(depth+1, fk, hm1updated, data, minconf, minlift)
        }
        return res
    }

    def rulegeneration(freq: Array[Array[Int]], data: Array[Array[Int]], minconf: Double, minlift: Double) = {
        var res = List[Rule]()
        freq.foreach(fk => {
            val h1 = fk.map(Array(_))
            res = res ++ apgenrules(0, fk, h1, data, minconf, minlift)
        })
        res
    }

    def apriorigen(frequent: Array[Array[Int]]): Array[Array[Int]] = {

        if (frequent.size == 0) 
            return new Array[Array[Int]](0)

        def combine(set1: Array[Int], set2: Array[Int], k: Int) = { 
            for (suffix1 <- set1.takeRight(set1.size-k); suffix2 <- set2.takeRight(set2.size-k)) yield set1.take(k) ++ Array(suffix1, suffix2)
        }

        def allSubsetsFrequent(frequent: Array[Array[Int]], candidate: Array[Int]): Boolean = {
            for (i <- 0 until candidate.size) {
                val ss = droppy(candidate, i)
                if (!frequent.map(itemset => itemset.deep == ss.deep).foldRight(false)( (a,b) => a || b)) 
                    return false
            }
            return true
        }
        
        val k = frequent(0).size - 1
        val loop = new Breaks
        var candidates = new Array[Array[Int]](0)
        for (i <- 0 until frequent.size) {
            loop.breakable {
                for (j <- i+1 until frequent.size) {
                    if ( frequent(i).take(k).deep == frequent(j).take(k).deep) {
                        val res = combine( frequent(i), frequent(j), k)
                        res.foreach(array => Sorting.quickSort(array))
                        candidates = candidates ++ res 
                    }  else {
                        loop.break
                    }
                }
            }
        }
        candidates.filter(candidate => allSubsetsFrequent(frequent, candidate))
    }

    def apriori(data: Array[Array[Int]], minSupport: Double, max: Int = 5): Array[Array[Int]] = {
        val courses = data.flatten.distinct.map(x => Array(x))
        Sorting.quickSort(courses)(Ordering.by[Array[Int],Int](_(0)))
        var fk = courses.filter(support(data, _) > minSupport)
        var res: Array[Array[Int]] = new Array[Array[Int]](0)
        var i = 0
        do {
            val candidates = apriorigen(fk)
            fk = candidates.filter(candidate => support(data, candidate) > minSupport)
            res = res++fk
            i = i + 1
            if (i >= max) 
                return res
        } while ( fk.size > 0 )
        res
    }
}
