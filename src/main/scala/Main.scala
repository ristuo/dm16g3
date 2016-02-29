package dm

object Main {

    def prereqRun(args: Array[String]) = {
        val data = DataReader.readCsv("data.csv")
        val coursecode = args(0).toInt
        val minconf = args(1).toDouble
        val maxrulesize = args(2).toInt
        val res = Prerequisites.rulesForPassing(data,coursecode,minconf,maxrulesize)
        res.foreach(println(_))
    }

    def main(args: Array[String]) {
        val commandsMap = Map("prereq" -> { prereqRun(_) })
        commandsMap(args(0))(args.tail)
    }
}
