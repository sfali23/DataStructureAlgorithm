import org.junit.runner.RunWith
import org.scalatest.{ BeforeAndAfterAll, FunSuite }
import org.scalatest.junit.JUnitRunner

/**
  * @author sali
  */
@RunWith(classOf[JUnitRunner])
class JobQueueTest extends FunSuite with BeforeAndAfterAll with JobQueueWork with TestHelper {}
