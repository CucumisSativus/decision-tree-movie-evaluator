package tul.poiis.decision_tree

/**
  * Created by michal on 10.01.2016.
  */
class Evaluation(val movie: Movie, val grade: Int) {
  def toPieceOfData: PieceOfData ={
    new PieceOfData(new Label(grade.toString), movie.toSamples.map(new Feature(_)).toArray)
  }
}
