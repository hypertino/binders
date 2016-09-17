import com.hypertino.inflector.naming.PlainConverter
import org.scalatest.{FlatSpec, Matchers}
import org.scalamock.scalatest.MockFactory

class TestBindArgsSpec extends FlatSpec with Matchers with MockFactory {
  "TestBindArgsSpec2 " should " should bind arguments by index" in {
    val stmt = mock[TestSerializer[PlainConverter.type]]

    inSequence {
      (stmt.writeInt _).expects(10)
      (stmt.writeIntNullable _).expects(Some(3))
      (stmt.writeIntNullable _).expects(None)
    }

    val noneInt: Option[Int] = None
    stmt.bindArgs(10, Some(3), noneInt)
  }
}
