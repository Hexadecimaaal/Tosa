package io.github.hexadecimaaal.tosa

import java.math.*

sealed class Expression {
  abstract fun reduce() : Expression
}

data class Numeral(val value : BigInteger) : Expression() {
  override fun reduce() = this
  override fun toString() = value.toString()
}

data class Addition(val lhs : Expression, val rhs : Expression) : Expression() {
  override fun reduce() =
      if (lhs is Numeral && rhs is Numeral) Numeral(lhs.value + rhs.value)
      else Addition(lhs.reduce(), rhs.reduce())

  override fun toString() = "$lhs + $rhs"
}

data class Power(val base : Expression, val exponent : Expression) : Expression() {
  override fun toString() = "$base ^ $exponent"
  override fun reduce() =
      if (base is Numeral && exponent is Numeral) Numeral(base.value.pow(exponent.value.toInt()))
      else if (exponent == BigInteger.valueOf(1)) base
      else Power(base.reduce(), exponent.reduce())
}

data class Multiplication(val lhs : Expression, val rhs : Expression) : Expression() {
  override fun toString() = "$lhs * $rhs"
  override fun reduce() =
      if (lhs is Numeral && rhs is Numeral) Numeral(lhs.value * rhs.value)
      else Multiplication(lhs.reduce(), rhs.reduce())
}

data class Symbol(val name : String) : Expression() {
  override fun toString() = name
  override fun reduce() = this
}

data class Product(
    val table : Map<Expression, BigInteger>,
    val constant : BigInteger = BigInteger.valueOf(1)
) : Expression() {

  override fun toString() : String {
    var res = if (constant != BigInteger.valueOf(1)) constant.toString() else ""
    for ((base, exponent) in table) {
      res += "* $base"
      if (exponent != BigInteger.valueOf(1)) res += "^ $exponent"
    }
    return res
  }

  override fun reduce() : Product {
    val map = mutableMapOf<Expression, BigInteger>()
    var const = this.constant
    for (factor in list.map(::simpl)) {
      if (factor is Numeral) const *= factor.value
      else if (factor is Power && factor.exponent is Numeral)
        map[factor] = map.getOrElse(factor, { BigInteger.valueOf(0) }) + factor.exponent.value
      else
        map[factor] = map.getOrElse(factor, { BigInteger.valueOf(0) }) + BigInteger.valueOf(1)
    }
    val res = mutableListOf<Expression>()
    for ((base, exponent) in map) {
      res.add(Power(base, Numeral(exponent)))
    }
    return Product(res, const)
  }
}

fun concatProducts(l : List<Product>) : Product {
  var const = BigInteger.valueOf(1)
  val result = mutableListOf<Expression>()
  for (product in l) {
    const *= product.constant
    result.addAll(product.list)
  }
  return Product(result, const)
}

fun productfromExpr(e : Expression) : Product {
  return when (e) {
    is Numeral -> Product(listOf(), e.value).reduce()
    is Addition, is Symbol, is Power -> Product(listOf(e)).reduce()
    is Multiplication -> Product(listOf(e.lhs, e.rhs)).reduce()
    is Product -> concatProducts(e.map(::productfromExpr)).let {
      Product(it.list, it.constant * e.constant)
    }
  }
}

tailrec fun simpl(e : Expression) : Expression =
    if (e == e.reduce()) e else simpl(e.reduce())

tailrec fun normalize(e : Expression) : Expression =
    if (e == productfromExpr(e)) e else normalize(productfromExpr(e))
