package tul.poiis.decision_tree


class PieceOfData(val label: Label, val fields: Array[Feature]) {
  def fieldValue(index: Int): String ={
    fields(index).stringValue
  }
}
