package DataBaseAccess
import scala.collection.immutable.ListMap
import scala.util.matching.Regex

object Assignment {
  val File_Location=io.Source.fromFile("C:\\Users\\thanup\\Downloads\\imdb_movies.csv")


  def main1(args: Array[String]): Unit = {
    println("Hello, Java!")
  }
  def main(args: Array[String]): Unit = {
    GetMoviebyDirector_Year("D.W. Griffith","1914","2000")
    getEnglishTitlesReport_ByUserReview(5)
    GetHighestBudget_Titles(1906,Country = "Australia")

  }
  def GetMoviebyDirector_Year(Director_name:String, Start_Year:String,End_Year:String): Unit ={
    var counter=0
    for(line<- File_Location.getLines()) {

      if (counter != 0 && counter <10000) {

        val cols = line.split(",(?=([^\\\"]*\\\"[^\\\"]*\\\")*[^\\\"]*$)").map(_.trim)
        val startYearInt = Start_Year.toInt
        val endYearInt = End_Year.toInt
        // println(s"${cols(9)}")

        if (cols(9) != "" && cols(3) != "") {
          val year = cols(3).toInt
          val list = cols(9)

          if (Director_name == cols(9) && (year >= startYearInt && year <= endYearInt)) {
            println(cols(1))

          }

        }
      }
      counter= counter +1
    }

  }
  def getEnglishTitlesReport_ByUserReview(Given_UserReview:Long): Unit ={
    var counter=0
    var Movie_Report:Map[String, Long] = Map()
    for(line<- File_Location.getLines()) {

      if (counter != 0 && counter <50) {
        println(line)
        val cols = line.split(",(?=([^\\\"]*\\\"[^\\\"]*\\\")*[^\\\"]*$)").map(_.trim)

        if (cols(20) != "" ) {
          val user_reviews = cols(20).toLong
          if (cols(8) != "" && (cols(8).toLowerCase).equals("english") && user_reviews >= Given_UserReview ) {
            Movie_Report += (cols(1) -> user_reviews)
          }
        }
      }
      counter= counter +1
    }
    //Sorting
    val result = ListMap(Movie_Report.toSeq.sortWith(_._2 > _._2):_*)
    for((k, v) <- result)
    {
      //where k is key and v is value
      print("Key:"+k+", ")
      println("Value:"+v)
    }
  }
  def GetHighestBudget_Titles(Given_Year:Int, Country:String): Unit ={
    var counter=0
    var result_MovieandBudgetDEtails :Map[String,Long]=Map()
    var MovieBudget_Titles:Map[String,Long]=Map()
    for(line<- File_Location.getLines()) {

      if (counter != 0 && counter <30) {

        val cols = line.split(",(?=([^\\\"]*\\\"[^\\\"]*\\\")*[^\\\"]*$)").map(_.trim)
        if (cols(16) != "" && cols(7) != "") {
          val YearInt = Given_Year.toInt
          if (cols(3).toInt == YearInt && cols(7).toLowerCase == Country.toLowerCase) {

            // println(budget)
            val budget = cols(16).split("\\d+")
            budget.mkString(cols(16))
            val x = new Regex("[^0-9.]")
            val result = x.replaceAllIn(cols(16), "").toLong

            println(result)
            MovieBudget_Titles += ( cols(1)-> result)
          }

        }
        result_MovieandBudgetDEtails = ListMap(MovieBudget_Titles.toSeq.sortWith(_._2 > _._2): _*)

      }
      counter = counter + 1
    }
    for ((k, v) <- result_MovieandBudgetDEtails) {
      //where k is key and v is value
      print("Key:" + k + ", ")
      println("Value:" + v)
    }
  }

}


