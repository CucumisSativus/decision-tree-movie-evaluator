package tul.poiis.decision_tree
import scala.math.log

object MainApp extends App{
  type DistributedDataPiece = (Label, Double)
  type DistributedData = List[DistributedDataPiece]


  def calculateDistribution(data: Array[PieceOfData]): DistributedData ={
    val allLabels = data.map(x => x.label)
    val labelsByCount = labelsWithCount(allLabels)
    labelsByCount.mapValues(x=> x/allLabels.length.toDouble).toList
  }

  def labelsWithCount(allLabels: Array[Label]): Map[Label, Int] = {
    allLabels.foldLeft(Map.empty[Label, Int]) { (m, x) => m + ((x, m.getOrElse(x, 0) + 1)) }
  }

  def entropy(distribution: DistributedData): Double ={
    distribution.map { symbol =>
      val probability = symbol._2
      probability * (log(probability) / log(2))
    }.sum * (-1)
  }

  def splitData(data: Array[PieceOfData], featureIndex: Int): Array[Array[PieceOfData]] ={
    val allPosibleValues = data.map(x => x.fieldValue(featureIndex)).distinct
    allPosibleValues.map { value =>
      data.filter(p => p.fieldValue(featureIndex) == value)
    }
  }

  def calculateEntropyGain(data: Array[PieceOfData], featureIndex: Int): Double ={
    val entropyGain = entropy(calculateDistribution(data))
    val splittedData = splitData(data, featureIndex)
    val entropyDecreases = splittedData.map( d => entropy(calculateDistribution(d)))
    entropyGain - entropyDecreases.sum
  }

  def homogeneous(data: Array[PieceOfData]): Boolean ={
    distinctLabels(data).length <= 1
  }

  def distinctLabels(data: Array[PieceOfData]): Array[Label] = {
    data.map(x => x.label).distinct
  }

  def majorityVote(data: Array[PieceOfData], node: Tree): Tree ={
    val labelsByCount = labelsWithCount(data.map( x=> x.label))
    val choosenLabel = labelsByCount.maxBy(_._2)._1
    node.copy(label = choosenLabel)
  }

  def buildDecisionTree(data: Array[PieceOfData], root: Tree, remainingFeatureIndices: Set[Int]): Tree ={
    if(homogeneous(data)){
      return root.copy(label = data(0).label)
    }

    if(remainingFeatureIndices.size == 0){
      return majorityVote(data, root)
    }

    val gains = remainingFeatureIndices.map(index =>(index, calculateEntropyGain(data, index)))
    val bestFeatureIndex = gains.maxBy(_._2)._1

    if(calculateEntropyGain(data, bestFeatureIndex) == 0){
      return majorityVote(data, root)
    }


    val splittedData = splitData(data, bestFeatureIndex)
    val children = splittedData.map { subset =>
      val child = Tree(parent = root, children = Array[Tree](), splitFeatureValue = subset(0).fieldValue(bestFeatureIndex))
      buildDecisionTree(subset, child, remainingFeatureIndices - bestFeatureIndex)
    }
    return root.copy(children = children)

  }

  def decisionTree(data: Array[PieceOfData]): Tree ={
    buildDecisionTree(data, Tree(null, Array[Tree]()), data(0).fields.indices.toSet)
  }

  override def main (args: Array[String]): Unit ={

  }
}
