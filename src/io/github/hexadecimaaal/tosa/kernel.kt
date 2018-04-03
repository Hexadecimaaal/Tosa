package io.github.hexadecimaaal.tosa

import java.math.*

sealed class Expression

data class Numeral(val value : BigInteger) : Expression() {
  override fun toString() : String {
    return value.toString()
  }
}

data class Addition(val lhs : Expression, val rhs : Expression) : Expression() {
  override fun toString() : String {
    return "$lhs + $rhs"
  }
}

data class Multiplication(val lhs : Expression, val rhs : Expression) : Expression() {
  override fun toString() : String {
    return "$lhs * $rhs"
  }
}

data class Enclosed(val expr : Expression) : Expression() {
  override fun toString() : String {
    return "($expr)"
  }
}

fun reduce(e : Expression) : Expression = when (e) {
  is Numeral -> e
  is Addition -> if (e.lhs is Numeral && e.rhs is Numeral) Numeral(e.lhs.value + e.rhs.value)
  else Addition(reduce(e.lhs), reduce(e.rhs))
  is Multiplication -> if (e.lhs is Numeral && e.rhs is Numeral) Numeral(e.lhs.value * e.rhs.value)
  else Multiplication(reduce(e.lhs), reduce(e.rhs))
  is Enclosed -> reduce(e.expr)
}

tailrec fun simpl(e : Expression) : Expression = if (e == reduce(e)) e else simpl(reduce(e))


