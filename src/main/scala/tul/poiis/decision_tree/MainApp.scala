package tul.poiis.decision_tree
import scala.math.log

object MainApp extends App{
  type DistributedDataPiece = (Label, Double)
  type DistributedData = List[DistributedDataPiece]


  def calculateDistribution(data: Array[PieceOfData]): DistributedData ={
    val allLabels = data.map(x => x.label)
    val labelsByCount = allLabels.foldLeft(Map.empty[Label, Int]) { (m, x) => m + ((x, m.getOrElse(x, 0) + 1)) }
    labelsByCount.mapValues(x=> x/allLabels.length.toDouble).toList
  }

  def entropy(distribution: DistributedData): Double ={
    distribution.map { symbol =>
      val probability = symbol._2
      probability * (log(probability) / log(2))
    }.sum * (-1)
  }

  def calculateEntropyGain(data: Array[PieceOfData], featureIndex: Int): Double ={
    val entropyGain = entropy(calculateDistribution(data))
    
  }


  override def main (args: Array[String]): Unit ={

  }
}
