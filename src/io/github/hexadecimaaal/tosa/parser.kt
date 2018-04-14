package io.github.hexadecimaaal.tosa

import java.math.BigInteger

sealed class Token

data class Number(private val value : BigInteger) : Token() {
  fun toNumeral() = Numeral(value)
  override fun toString() : String = "integer '$value'"
}

object Plus : Token() {
  override fun toString() : String = "plus '+'"
}

object Minus : Token() {
  override fun toString() : String = "minus '-'"
}

object Times : Token() {
  override fun toString() : String = "times '*'"
}

object Slash : Token() {
  override fun toString() : String = "slash '/'"
}

object Caret : Token() {
  override fun toString() : String = "caret '^'"
}

object LP : Token() {
  override fun toString() : String = "left parenthesis '('"
}

object RP : Token() {
  override fun toString() : String = "right parenthesis ')'"
}

object END : Token() {
  override fun toString() : String = "end of input"
}

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
      else -> throw ParseException("cannot read input char '${input[pos]}'")
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
      LP -> parseFactor()
      END -> Numeral(BigInteger.valueOf(0))
      is Identifier -> parseFactor()
      else -> throw ParseException("unexpected token \"$x\"")
    }
  }

  private fun parseRP() {
    val x = getToken()
    when (x) {
      RP -> return
      else -> throw ParseException("unexpected token \"$x\"")
    }
  }

  private fun parseEnclosed() : Expression {
    val x = getToken()
    return when (x) {
      is Number -> x.toNumeral()
      LP -> {
        val x1 = parse()
        parseRP()
        x1
      }
      is Identifier -> x.toSymbol()
      else -> throw ParseException("unexpected token \"$x\"")
    }
  }

  private fun parseFactor() : Expression {
    val left = parseEnclosed()
    val x = getToken()
    return when (x) {
      Plus, Minus -> {
        ungetToken()
        parseExprEx(left)
      }
      Times, Slash -> {
        ungetToken()
        parseTermEx(left)
      }
      Caret -> {
        Power(left, parseFactor())
      }
      END -> {
        ungetToken()
        left
      }
      is Identifier -> {
        ungetToken()
        parseTermEx(left)
      }
      else -> throw ParseException("unexpected token \"$x\"")
    }
  }

  private tailrec fun parseTermEx(left : Expression) : Expression {
    val x = getToken()
    return when (x) {
      Plus, Minus -> {
        ungetToken()
        parseExprEx(left)
      }
      Times -> {
        val n = parseFactor()
        parseTermEx(Multiplication(left, n))
      }
      Slash -> {
        val n = parseFactor()
        parseTermEx(Multiplication(left, Multiplication(n, Numeral.MINUS_ONE)))
      }
      RP -> {
        ungetToken()
        left
      }
      END -> left
      is Identifier -> {
        parseTermEx(Multiplication(left, x.toSymbol()))
      }
      else -> throw ParseException("unexpected token \"$x\"")
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
      Plus -> {
        val n = parseFactor()
        Addition(left, parseTermEx(n))
      }
      Minus -> {
        val n = parseFactor()
        Addition(left, Multiplication(parseTermEx(n), Numeral.MINUS_ONE))
      }
      RP -> {
        ungetToken()
        left
      }
      END -> left
      else -> throw ParseException("unexpected token \"$x\"")
    }
  }
}

fun parse(i : String) : Expression {
  val p = Parser(i)
  return p.parse()
}

class ParseException(override val message : String = "") : Exception(message)
