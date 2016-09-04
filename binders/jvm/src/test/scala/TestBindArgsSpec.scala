import com.hypertino.inflector.naming.PlainConverter
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}

class TestBindArgsSpec extends FlatSpec with Matchers {
  "TestBindArgsSpec " should " should bind arguments by index" in {
    val stmt = mock[TestSerializer[PlainConverter.type]]
    val noneInt: Option[Int] = None
    stmt.bindArgs(10, Some(3), noneInt)
    verify(stmt).writeInt(10)
    verify(stmt).writeIntNullable(Some(3))
    verify(stmt).writeIntNullable(None)
  }
}
