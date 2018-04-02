package io.github.hexadecimaaal.tosa

import java.math.BigInteger

sealed class Token
data class Number(val value : BigInteger) : Token()
object Plus : Token()
object Times : Token()
object LP : Token()
object RP : Token()

class Parser(val input : String) {
    private var pos : Int = 0
    fun getToken() : Token = TODO()
    fun ungetToken() : Unit = TODO()
    fun parse() : Expression {
        val x = getToken()
        return when (x) {
            is Number -> parseTermEx(Numeral(x.value))
            Plus -> TODO()
            Times -> TODO()
            LP -> {
                ungetToken()
                parseEnclosed()
            }
            RP -> TODO()
        }
    }

    fun parseRP() {
        when (getToken()) {
            is Number -> TODO()
            Plus -> TODO()
            Times -> TODO()
            LP -> TODO()
            RP -> return
        }
    }

    fun parseEnclosed() : Expression {
        val x = getToken()
        return when (x) {
            is Number -> Numeral(x.value)
            Plus -> TODO()
            Times -> TODO()
            LP -> {
                val x = parse()
                parseRP()
                x
            }
            RP -> TODO()
        }
    }

    tailrec fun parseTermEx(left : Expression) : Expression {
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
        }
    }

    tailrec fun rightMost(expr : Expression) : Expression = when (expr) {
        is Numeral -> expr
        is Addition -> rightMost(expr.rhs)
        is Multiplication -> rightMost(expr.rhs)
        is Enclosed -> expr
    }

    tailrec fun parseExprEx(left : Expression) : Expression {
        val x = getToken()
        return when (x) {
            is Number -> TODO()
            Plus -> {
                val n = parseEnclosed()
                parseExprEx(Addition(left, n))
            }
            Times -> {
                ungetToken()
                parseTermEx(rightMost(left))
            }
            LP -> TODO()
            RP -> {
                ungetToken()
                left
            }
        }
    }
}
