package dm

object Main {
    def main(args: Array[String]) {
        val data = DataReader.readCsv("data.csv")
        //Week5.toCategories(data)
        Group2.grouping(data)
    }
}
