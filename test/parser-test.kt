import io.github.hexadecimaaal.tosa.*
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.math.BigInteger

class ParserTests {
    @Test fun basicExpr() {
        val p = Parser("1+1")
        assertEquals(p.parse(), Addition(Numeral(BigInteger.valueOf(1)),
                Numeral(BigInteger.valueOf(1))))
    }
    @Test fun enclosed() {
        val p = Parser("(1+1)")
        assertEquals(p.parse(), Enclosed(
                Addition(Numeral(BigInteger.valueOf(1)),
                Numeral(BigInteger.valueOf(1)))))
    }
    @Test fun hybrid() {
        val p = Parser("1+2*3")
        assertEquals(p.parse(), Addition(
                Numeral(BigInteger.valueOf(1)),
                Multiplication(
                        Numeral(BigInteger.valueOf(2)),
                        Numeral(BigInteger.valueOf(3))
                )))
    }
    @Test fun hybrid2() {
        val p = Parser("(1+2)*3")
        assertEquals(p.parse(), Multiplication(
                Enclosed(Addition(
                        Numeral(BigInteger.valueOf(1)),
                        Numeral(BigInteger.valueOf(2))
                )),
                Numeral(BigInteger.valueOf(3))))
    }
    @Test fun empty() {
        val p = Parser("")
        assertEquals(p.parse(), Numeral(BigInteger.valueOf(0)))
    }
}
