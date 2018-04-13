package io.github.hexadecimaaal.tosa

import java.math.BigInteger

sealed class Token

data class Number(private val value : BigInteger) : Token() {
  fun toNumeral() = Numeral(value)
}

object Plus : Token()
object Times : Token()
object Caret : Token()
object LP : Token()
object RP : Token()
object END : Token()

data class Identifier(private val name : String) : Token() {
  fun toSymbol() = Symbol(name)
}

class Parser(i : String) {
  private var pos : Int = 0
  private var lpos : Int = pos
  private var input : String = i + "\uffff"

  companion object {
    const val numbers = "1234567890"
    const val AZaz = "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"
    const val AZaz09 = "$AZaz-1234567890"
    const val Space = " \n\t\r"
  }

  private fun eatIdentifier() : Identifier {
    var s = ""
    while (input[pos] in AZaz09) {
      s += input[pos]
      pos++
    }
    return Identifier(s)
  }

  private tailrec fun getToken() : Token {
    lpos = pos
    return when (input[pos]) {
      in numbers -> readNum()
      '+' -> {
        pos++
        Plus
      }
      '*' -> {
        pos++
        Times
      }
      '^' -> {
        pos++
        Caret
      }
      '(' -> {
        pos++
        LP
      }
      ')' -> {
        pos++
        RP
      }
      '\uffff' -> {
        pos++
        END
      }
      in AZaz -> eatIdentifier()
      in Space -> {
        pos++
        getToken()
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
    ungetToken()
    return when (x) {
      is Number -> parseFactor()
      Plus -> TODO()
      Times -> TODO()
      LP -> parseFactor()
      RP -> TODO()
      END -> Numeral(BigInteger.valueOf(0))
      is Identifier -> parseFactor()
      Caret -> TODO()
    }
  }

  private fun parseRP() {
    when (getToken()) {
      is Number -> TODO()
      Plus -> TODO()
      Times -> TODO()
      LP -> TODO()
      RP -> return
      END -> TODO()
      Caret -> TODO()
      is Identifier -> TODO()
    }
  }

  private fun parseEnclosed() : Expression {
    val x = getToken()
    return when (x) {
      is Number -> x.toNumeral()
      Plus -> TODO()
      Times -> TODO()
      LP -> {
        val x1 = parse()
        parseRP()
        x1
      }
      RP -> TODO()
      END -> TODO()
      is Identifier -> x.toSymbol()
      Caret -> TODO()
    }
  }

  private fun parseFactor() : Expression {
    val left = parseEnclosed()
    val x = getToken()
    return when (x) {
      is Number -> TODO()
      Plus -> {
        ungetToken()
        parseExprEx(left)
      }
      Times -> {
        ungetToken()
        parseTermEx(left)
      }
      Caret -> {
        Power(left, parseFactor())
      }
      LP -> TODO()
      RP -> TODO()
      END -> {
        ungetToken()
        left
      }
      is Identifier -> {
        ungetToken()
        parseTermEx(left)
      }
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
        val n = parseFactor()
        parseTermEx(Multiplication(left, n))
      }
      LP -> TODO()
      RP -> {
        ungetToken()
        left
      }
      END -> left
      is Identifier -> {
        parseTermEx(Multiplication(left, x.toSymbol()))
      }
      Caret -> TODO()
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
        val n = parseFactor()
        Addition(left, parseTermEx(n))
      }
      Times -> TODO()
      LP -> TODO()
      RP -> {
        ungetToken()
        left
      }
      END -> left
      is Identifier -> TODO()
      Caret -> TODO()
    }
  }
}

fun parse(i : String) : Expression {
  val p = Parser(i)
  return p.parse()
}
