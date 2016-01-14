package tul.poiis.decision_tree

import java.io.{PrintWriter, File}

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

  def majorityVoteLabel(data: Array[PieceOfData]): Label ={
    val labelsByCount = labelsWithCount(data.map( x=> x.label))
    labelsByCount.maxBy(_._2)._1
  }
  def majorityVote(data: Array[PieceOfData], node: Tree): Tree ={
    val choosenLabel = majorityVoteLabel(data)
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
      val child = Tree(parent = root, children = Array[Tree](), splitFeatureValue = subset(0).fieldValue(bestFeatureIndex), splitFeature = new SplitFeature(bestFeatureIndex), label = majorityVoteLabel(subset))
      buildDecisionTree(subset, child, remainingFeatureIndices - bestFeatureIndex)
    }
    return root.copy(children = children, splitFeature = SplitFeature(bestFeatureIndex))

  }

  def decisionTree(data: Array[PieceOfData]): Tree ={
    buildDecisionTree(data, Tree(null, Array[Tree]()), data(0).fields.indices.toSet)
  }

  def classifyPieceOfData(tree: Tree, pieceOfData: PieceOfData): Label ={
    if(tree.children.isEmpty){
      return tree.label
    }
    else{
      val properChildren = tree.children.filter(child => child.splitFeatureValue == pieceOfData.fieldValue(tree.splitFeature.index))
      if(properChildren.size > 0 ) {
        return classifyPieceOfData(properChildren(0), pieceOfData);
      }
      else{
        return tree.children.sortWith(_.children.size < _.children.size).reverse.map(_.label).head
      }
    }
  }

  def decisionTreeForUsers(evaluationByUser: Map[Int, List[Evaluation]]): Map[Int, Tree] ={
    evaluationByUser.par.map { case(userId, evaluations) =>
      //println(s"Preparing tree for user ${userId}")
      (userId, decisionTree(evaluations.map(_.toPieceOfData).toArray))
    }.seq
  }

  def predictionLine(evalId:Int, userId:Int, movieId: Int, evaluation: Int): String ={
    "%d;%d;%d;%d".format(evalId, userId, movieId, evaluation)
  }

  def obtainPrediction(evalId: Int, userId: Int, movieId: Int, moviesMap: Map[Int, Movie], treesByUser: Map[Int, Tree]): String ={
    //println(s"Obtaining prediction for eval ${evalId} userId ${userId} movieId ${movieId}")
    val movie = moviesMap(movieId)
    val properTree = treesByUser(userId)
    val treeResult = classifyPieceOfData(properTree, new PieceOfData(new Label(""), movie.toSamples.map(new Feature(_)).toArray))
    predictionLine(evalId, userId, movieId, treeResult.name.toInt)
  }

  def printTree(tree: Tree): Unit ={
	//printIndent(tree.splitFeature.index)
	println("----------")
	//printIndent(tree.splitFeature.index)
	if (tree.label != null) {
		println("label: " + tree.label.name)
	}
	//printIndent(tree.splitFeature.index)
	println("split ind: " + tree.splitFeature.index)
	//printIndent(tree.splitFeature.index)
	println("split val: " + tree.splitFeatureValue)
	//println("child num: " + tree.children.length)
	for( i <- 0 until tree.children.length) {
		printTree(tree.children(i))
	}
  }

  def printIndent(n: Int): Unit ={
	var i=0
	for (i <- 1 to n) {
	  print("    ")
	}
  }

  override def main (args: Array[String]): Unit ={
    val moviesMap = InputParsers.readMoviesFile(args(0))
    println("Movies map prepared")
    val usersEvaluation = InputParsers.readTrainSetFile(args(1), moviesMap)
    println("User evaluations prepared")
    val userTrees = decisionTreeForUsers(usersEvaluation)
    val unknows = InputParsers.readUnknowns(args(2))
    val resultLines = unknows.par.map(u => obtainPrediction(u._1, u._2, u._3, moviesMap, userTrees)).seq
    val pw = new PrintWriter(new File(args(3)))
    resultLines.foreach{ line =>
      pw.write(line)
      pw.write("\n")
    }
    pw.close()
	printTree(userTrees.get(69).get)
  }
}
