package tul.poiis.decision_tree

abstract class MovieField {
  def toDouble: Double
  def toDecisionTreeField: String
  def fixDecimalPoin(string: String) = string.replaceAll(",", ".")
  def fieldIndex: Int
}

class BudgetField(budget: String) extends MovieField{
  val windowSize = 5000000
  val numWindows = 20
  override def toDouble: Double = fixDecimalPoin(budget).toDouble

  def budgetWindows(): Array[String] ={

    Array.tabulate(numWindows)(e=> s"${e *windowSize}-${(e+1) *windowSize}")
  }
  override def toDecisionTreeField: String = {
    val index = ((budget.toDouble/(windowSize*10))-1).toInt
    if(index == -1){
      budgetWindows()(0)
    }
    else {
      budgetWindows()(index)
    }
  }

  override def fieldIndex: Int = 0
}

class PopularityField(popularity: String) extends MovieField{
  val maxPopularity = 10.0
  val popularityWindowSize = 1.0

  override def toDouble: Double = fixDecimalPoin(popularity).toDouble

  def popularityWindows(): Array[String] = {
    Array.tabulate((maxPopularity/popularityWindowSize).toInt +1)(e=> s"${e *popularityWindowSize}-${(e+1) *popularityWindowSize}")
  }
  override def toDecisionTreeField: String = {
    popularityWindows()((toDouble / popularityWindowSize).toInt)
  }

  override def fieldIndex: Int = 1
}

class VoteAverageField(average: String) extends MovieField{
  val maxVote = 10.0
  val voteWindowSize = 1.0
  override def toDouble: Double = fixDecimalPoin(average).toDouble

  def voteWindows(): Array[String] = {
    Array.tabulate((maxVote/voteWindowSize).toInt +1)(e=> s"${e *voteWindowSize}-${(e+1) *voteWindowSize}")
  }
  override def toDecisionTreeField: String = {
    voteWindows()((toDouble / voteWindowSize).toInt)
  }

  override def fieldIndex: Int = 2
}

class ReleaseYearField(date: String ) extends MovieField{
  val maxYear = 2015
  val minYear = 1900
  val yearWindowSize = 20

  override def toDouble: Double = date.split('.').last.toDouble

  def year: Int = date.split('.').last.toInt

  def yearWindows(): Array[String] ={
    Array.tabulate(((maxYear - minYear)/yearWindowSize) + 1)(e=> s"${(minYear + (e *yearWindowSize))}-${minYear+ (e+1) *yearWindowSize}")
  }
  override def toDecisionTreeField: String = {
    yearWindows()((year - minYear)/yearWindowSize)
  }

  override def fieldIndex: Int = 3
}
