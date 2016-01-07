package tul.poiis.decision_tree

class SplitFeature {}

class SplitValue{}

class Tree(val parent: Tree,
           val children: Array[Tree],
           val splitFeature: SplitFeature,
           val splitFeatureValue: SplitValue,
           val label: Label
          ) {

}
