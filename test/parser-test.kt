import io.github.hexadecimaaal.tosa.*
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.math.BigInteger

class ParserTests {
  @Test
  fun basicExpr() {
    assertEquals(
        Addition(
            Numeral(BigInteger.valueOf(1)),
            Numeral(BigInteger.valueOf(1))),
        parse("1+1"))
  }

  @Test
  fun enclosed() {
    assertEquals(
        Addition(Numeral(BigInteger.valueOf(1)),
            Numeral(BigInteger.valueOf(1))),
        parse("(1+1)"))
  }

  @Test
  fun hybrid() {
    assertEquals(
        Addition(
            Numeral(BigInteger.valueOf(1)),
            Multiplication(
                Numeral(BigInteger.valueOf(2)),
                Numeral(BigInteger.valueOf(3))
            )),
        parse("1+2*3"))
  }

  @Test
  fun hybrid2() {
    assertEquals(
        Multiplication(
            Addition(
                Numeral(BigInteger.valueOf(1)),
                Numeral(BigInteger.valueOf(2))
            ),
            Numeral(BigInteger.valueOf(3))),
        parse("(1+2)*3"))
  }

  @Test
  fun empty() {
    assertEquals(
        Numeral(BigInteger.valueOf(0)),
        parse(""))
  }
}
