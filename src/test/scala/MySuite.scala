import zio.*
import zio.test.*

object MySuite extends ZIOSpecDefault:
  val spec = suite("a suite")(
    test("example test that succeeds") {
      val obtained = 42
      val expected = 42
      assertTrue(obtained == expected)
    }
  )
