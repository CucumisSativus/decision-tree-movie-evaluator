package tul.poiis.decision_tree

/**
  * Created by michal on 10.01.2016.
  */
class Movie(fields: List[MovieField]) {
  def toSamples = fields.sortBy(_.fieldIndex).map(_.toDecisionTreeField)
}
