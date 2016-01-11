package tul.poiis.decision_tree
import java.io._
import com.github.tototoshi.csv._

object MyCsvReader {
  implicit object MyCsvReader extends DefaultCSVFormat {
    override val delimiter = ';'
  }

  def reader(filePath: String) = CSVReader.open(new File(filePath))
}
