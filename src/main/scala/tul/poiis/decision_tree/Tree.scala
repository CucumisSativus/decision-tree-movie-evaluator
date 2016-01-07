package tul.poiis.decision_tree

case class SplitFeature(val index: Int) {}


case class Tree(val parent: Tree,
           val children: Array[Tree],
           val splitFeature: SplitFeature = null,
           val splitFeatureValue: String = null,
           val label: Label = null
          ) {

}
