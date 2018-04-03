package io.github.hexadecimaaal.tosa

import java.math.BigInteger

sealed class Token
data class Number(val value : BigInteger) : Token()
object Plus : Token()
object Times : Token()
object LP : Token()
object RP : Token()
object END : Token()

class Parser(i : String) {
  private var pos : Int = 0
  private var lpos : Int = pos
  private var input : String = i + "\uffff"

  companion object {
    const val numbers = "1234567890"
  }

  private fun getToken() : Token {
    lpos = pos
    return when (input[pos]) {
      in numbers -> readNum()
      '+' -> {
        pos++; Plus
      }
      '*' -> {
        pos++; Times
      }
      '(' -> {
        pos++; LP
      }
      ')' -> {
        pos++; RP
      }
      '\uffff' -> {
        pos++; END
      }
      else -> TODO()
    }
  }

  private fun readNum() : Number {
    while (input[pos] in numbers) pos++
    return Number(BigInteger(input.slice(IntRange(lpos, pos - 1))))
  }

  private fun ungetToken() {
    pos = lpos
  }

  fun parse() : Expression {
    val x = getToken()
    return when (x) {
      is Number -> parseTermEx(Numeral(x.value))
      Plus -> TODO()
      Times -> TODO()
      LP -> {
        ungetToken()
        parseTermEx(parseEnclosed())
      }
      RP -> TODO()
      END -> Numeral(BigInteger.valueOf(0))
    }
  }

  private fun parseRP() {
    when (getToken()) {
      is Number -> TODO()
      Plus -> TODO()
      Times -> TODO()
      LP -> TODO()
      RP -> return
    }
  }

  private fun parseEnclosed() : Expression {
    val x = getToken()
    return when (x) {
      is Number -> Numeral(x.value)
      Plus -> TODO()
      Times -> TODO()
      LP -> {
        val x = parse()
        parseRP()
        Enclosed(x)
      }
      RP -> TODO()
      END -> TODO()
    }
  }

  private tailrec fun parseTermEx(left : Expression) : Expression {
    val x = getToken()
    return when (x) {
      is Number -> TODO()
      Plus -> {
        ungetToken()
        parseExprEx(left)
      }
      Times -> {
        val n = parseEnclosed()
        parseTermEx(Multiplication(left, n))
      }
      LP -> TODO()
      RP -> {
        ungetToken()
        left
      }
      END -> left
    }
  }

//    tailrec fun rightMost(expr : Expression) : Expression = when (expr) {
//        is Numeral -> expr
//        is Addition -> rightMost(expr.rhs)
//        is Multiplication -> rightMost(expr.rhs)
//        is Enclosed -> expr
//    }

  private fun parseExprEx(left : Expression) : Expression {
    val x = getToken()
    return when (x) {
      is Number -> TODO()
      Plus -> {
        val n = parseEnclosed()
        Addition(left, parseTermEx(n))
      }
      Times -> TODO()
      LP -> TODO()
      RP -> {
        ungetToken()
        left
      }
      END -> left
    }
  }
}

fun parse(i : String) : Expression {
  val p = Parser(i)
  return p.parse()
}
