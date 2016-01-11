package tul.poiis.decision_tree

import scala.collection.mutable.ListBuffer

object InputParsers {
  def parseMovieEntry(csv_entry: Map[String, String]): (Int, Movie) = {
    val fieldsList = ListBuffer[MovieField]()
    fieldsList += new PopularityField(csv_entry("popularity"))
    fieldsList += new BudgetField(csv_entry("budget"))
    fieldsList += new VoteAverageField(csv_entry("vote_average"))
    fieldsList += new ReleaseYearField(csv_entry("release_date"))
    (csv_entry("Id").toInt, new Movie(fieldsList.toList))
  }

  def readMoviesFile(filepath: String) : Map[Int, Movie] ={
    val reader = MyCsvReader.reader(filepath)
    val csv_list = reader.allWithHeaders()
    reader.close()
    csv_list.map { entry =>
      val parseResult: (Int, Movie) = parseMovieEntry(entry)
      parseResult._1 -> parseResult._2
    }(collection.breakOut): Map[Int, Movie]
  }

  def parseTrainEntry(csv_entry: List[String], moviesMap: Map[Int, Movie]): (Int, Evaluation) ={
    val personId = csv_entry(1)
    val movieId = csv_entry(2)
    val grade = csv_entry(3)
    (personId.toInt, new Evaluation(grade = grade.toInt, movie = moviesMap(movieId.toInt)))
  }

  def readTrainSetFile(filepath: String, moviesMap: Map[Int, Movie]): Map[Int, List[Evaluation]] ={
    val reader = MyCsvReader.reader(filepath)
    val csv_list: List[List[String]] = reader.all()
    reader.close()
    val parsedTuples = csv_list.map { entry =>
      val parseResult: (Int, Evaluation) = parseTrainEntry(entry, moviesMap)
      parseResult._1 -> parseResult._2
    }
    parsedTuples.groupBy(_._1).mapValues(_.map(_._2))
  }

  def parseUnknownEntry(csv_entry: List[String]): (Int, Int, Int) ={
    val evalId = csv_entry(0)
    val personId = csv_entry(1)
    val movieId = csv_entry(2)
    (evalId.toInt, personId.toInt, movieId.toInt)
  }

  def readUnknowns(filepath: String): List[(Int, Int, Int)] ={
    val reader = MyCsvReader.reader(filepath)
    val csv_list: List[List[String]] = reader.all()
    reader.close()
    csv_list.map { entry =>
      parseUnknownEntry(entry)
    }
  }
}
