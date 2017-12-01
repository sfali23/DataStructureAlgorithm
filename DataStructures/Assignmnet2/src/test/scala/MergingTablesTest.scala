import org.scalatest.FunSuite

/**
  * @author sali
  */
class MergingTablesTest extends FunSuite with MergingTablesWork with TestHelper {
  override val testDataFolderPath: String = "merge-tables"
}
