package io.github.hexadecimaaal.tosa

import java.math.BigInteger

sealed class Token

data class Number(private val value : BigInteger) : Token() {
  fun toNumeral() = Numeral(value)
  override fun toString() : String = "integer $value"
}

object Plus : Token() {
  override fun toString() : String = "plus +"
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

object BackSlash : Token() {
  override fun toString() : String = "backslash '\\'"
}

object Dot : Token() {
  override fun toString() : String {
    return "dot '.'"
  }
}

data class Identifier(private val name : String) : Token() {
  fun toSymbol() = Symbol(name)
}

class OldParser(i : String) {
  private var pos : Int = 0
  private var lpos : Int = pos
  private var input : String = i + "\u0000"

  companion object {
    const val numbers = "1234567890"
    const val AZaz = "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"
    const val AZaz09 = "$AZaz-1234567890"
    const val Space = " \n\t\r"
  }

  private fun getIdentifier() : Identifier {
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
      '+'        -> {
        pos++
        Plus
      }
      '-'        -> {
        pos++
        Minus
      }
      '*'        -> {
        pos++
        Times
      }
      '/'        -> {
        pos++
        Slash
      }
      '\\'       -> {
        pos++
        BackSlash
      }
      '.'        -> {
        pos++
        Dot
      }
      '^'        -> {
        pos++
        Caret
      }
      '('        -> {
        pos++
        LP
      }
      ')'        -> {
        pos++
        RP
      }
      '\u0000'   -> {
        pos++
        END
      }
      in AZaz    -> getIdentifier()
      in Space   -> {
        pos++
        getToken()
      }
      else       -> throw ParseException("cannot read input char '${input[pos]}'")
    }
  }

  private fun readNum() : Number {
    while (input[pos] in numbers) pos++
    return Number(BigInteger(input.slice(IntRange(lpos, pos - 1))))
  }

  private fun ungetToken() {
    check(pos != lpos) { "can't retract twice / position is 0" }
    pos = lpos
  }

  fun parse() : Expression {
    val x = getToken()
    ungetToken()
    return when (x) {
      is Number, is Identifier, LP -> parseExpr()
      END                          -> Numeral(BigInteger.valueOf(0))
      else                         -> throw ParseException("unexpected token \"$x\"")
    }
  }

  private fun parseRP() {
    when (val x = getToken()) {
      RP   -> return
      else -> throw ParseException("unexpected token \"$x\"")
    }
  }

  private fun parseAtom() : Expression {
    return when (val x = getToken()) {
      is Number     -> x.toNumeral()
      LP            -> {
        val x1 = parse()
        parseRP()
        x1
      }
      is Identifier -> Pure(x.toSymbol())
      else          -> throw ParseException("unexpected token \"$x\"")
    }
  }

  private fun parseFactorEx(left : MathExpression) : MathExpression {
    return when (val x = getToken()) {
      Caret                                                 -> {
        Power(left, parseFactor())
      }
      END, is Identifier, Plus, Minus, Times, Slash, LP, RP -> {
        ungetToken()
        left
      }
      else                                                  ->
        throw ParseException("unexpected token \"$x\"")
    }
  }

  private tailrec fun parseTermEx(left : MathExpression) : MathExpression {
    return when (val x = getToken()) {
      Plus, Minus       -> {
        ungetToken()
        left
      }
      Times             -> {
        val n = parseFactor()
        parseTermEx(left * n)
      }
      Slash             -> {
        val n = parseFactor()
        parseTermEx(left * Power(n, Numeral.MINUS_ONE))
      }
      RP                -> {
        ungetToken()
        left
      }
      END               -> {
        ungetToken()
        left
      }
      is Identifier, LP -> {
        ungetToken()
        val n = parseFactor()
        parseTermEx(left * n)
      }
      else              -> throw ParseException("unexpected token \"$x\"")
    }
  }

  private tailrec fun parseExprEx(left : MathExpression) : MathExpression {
    return when (val x = getToken()) {
      Plus  -> {
        val n = parseTerm()
        parseExprEx(left + n)
      }
      Minus -> {
        val n = parseTerm()
        parseExprEx(left + n * Numeral.MINUS_ONE)
      }
      RP    -> {
        ungetToken()
        left
      }
      END   -> {
        ungetToken()
        left
      }
      else  -> throw ParseException("unexpected token \"$x\"")
    }
  }

  private fun parseFactor() = parseFactorEx(parseAtom().toMath())
  private fun parseTerm() = parseTermEx(parseFactor())
  private fun parseExpr() = parseExprEx(parseTerm())
}

fun parse(i : String) : Expression {
  val p = OldParser(i)
  return p.parse()
}

class ParseException(override val message : String = "") : Exception(message)


class Parser {

}
