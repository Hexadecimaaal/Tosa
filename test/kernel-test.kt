import io.github.hexadecimaaal.tosa.*
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class KernelTests {
  @Test
  fun simpl1() {
    assertEquals(parse("2"), simpl(parse("1+1")))
  }

  @Test
  fun simpl2() {
    assertEquals(parse("6"), simpl(parse("1+(2+3)")))
  }

  @Test
  fun simpl3() {
    assertEquals(
        simpl(parse("12345+54321")),
        simpl(parse("14325+52341")))
  }

  @Test
  fun simpl4() {
    assertEquals(
        simpl(parse("3*3*3")),
        simpl(parse("20+3+4"))
    )
  }
}
