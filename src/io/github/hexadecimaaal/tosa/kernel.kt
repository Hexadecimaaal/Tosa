package io.github.hexadecimaaal.tosa

import java.math.*

sealed class Expression {
  abstract fun reduce() : Expression
  open operator fun plus(e : Expression) : Addition = Addition(this, e)
  open operator fun times(e : Expression) : Multiplication = Multiplication(this, e)
}

data class Numeral(val value : BigInteger) : Expression() {
  override fun reduce() = this
  override fun toString() = value.toString()

  companion object {
    val ONE = Numeral(BigInteger.ONE)
    val ZERO = Numeral(BigInteger.ZERO)
  }

  operator fun plus(e : Numeral) : Numeral =
      Numeral(value + e.value)
  operator fun times(e : Numeral) : Numeral =
      Numeral(value * e.value)
  fun power(e : Numeral) : Numeral =
      Numeral(value.pow(e.value.toInt()))

  fun toSum() : Sum = Sum(mapOf(), this)
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
      else if (exponent == BigInteger.ONE) base
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
    val table : Map<Expression, Sum>,
    val constant : Numeral = Numeral.ONE
) : Expression() {
  override fun reduce() : Expression = this

  override fun toString() : String {
    var res = constant.toString()
    for ((base, exponent) in table) {
      res += "* $base"
      if (!(exponent.table.isEmpty() && exponent.constant == Numeral.ONE))
        res += "^ $exponent"
    }
    return res
  }

  fun combine(pp : Product, mult : Sum = Numeral.ONE.toSum()) : Product {
    val p = pp.distrib(mult)
    val result = table.toMutableMap()
    for ((base, exp) in p.table) {
      result[base] = result[base]?.combine(exp) ?: exp
    }
    return Product(result, constant + p.constant)
  }

  fun combine(e : Expression, exp : Sum = Numeral.ONE.toSum()) : Product {
    if (e is Numeral) return Product(table, constant * e)
    val result = table.toMutableMap()
    result[e] = result[e]?.combine(exp) ?: exp
    return Product(result, constant)
  }

  fun distrib(exp : Expression) : Product =
      Product(table.mapValues {
        it.value.distrib(exp)
      }, constant)

  fun distrib(exp : Numeral) : Product = Product(table.mapValues {
    it.value.distrib(exp)
  }, constant)

  fun distrib(exp : Sum) : Product {
    val result = Product(mutableMapOf(), constant.power(exp.constant))
    for((base, multplicator) in exp.table)
      result.combine(distrib(base).distrib(multplicator))
    for((base, multplicator) in exp.table)
      result.combine(Power(base, base * multplicator))
    return result
  }

  fun flatten() : Product {
    val result = Product(mapOf(), constant)
    for ((base, exp) in table) when (base) {
      is Sum -> result.combine(base.flatten(), exp)
      is Product -> result.combine(base.flatten(), exp)
      else -> result.combine(base, exp)
    }
    return result
  }
}

data class Sum(
    val table : Map<Expression, Numeral>,
    val constant : Numeral = Numeral.ZERO
) : Expression() {
  override fun reduce() : Expression = this

  override fun toString() : String {
    var res = ""
    for ((base, mult) in table) {
      if (mult != Numeral.ONE) res += "$mult * "
      res += "$base +"
    }
    res += constant.toString()
    return res
  }

  fun combine(s : Sum, mult : Numeral = Numeral.ONE) : Sum {
    val p = s.distrib(mult)
    val result = table.toMutableMap()
    for ((base, t) in p.table)
      result[base] = result[base]?.plus(t) ?: t
    return Sum(result, constant.plus(p.constant))
  }

  fun combine(e : Expression, mult : Numeral = Numeral.ONE) : Sum {
    if (e is Numeral) return Sum(table, constant + e)
    val result = table.toMutableMap()
    result[e] = result[e]?.plus(mult) ?: mult
    return Sum(result, constant)
  }
  
  fun distrib(mult : Expression) : Sum =
      Sum(table.mapKeys {
        it.key * mult
      })

  fun distrib(mult : Numeral) : Sum =
      Sum(table.mapValues {
        it.value * mult
      })
  
  fun distrib(mult : Sum) : Sum {
    val result = Sum(mutableMapOf(), mult.constant * constant)
    for((base, multplicator) in mult.table)
      result.combine(distrib(base).distrib(multplicator))
    for((base, multplicator) in mult.table)
      result.combine(base * multplicator * constant)
    return result
  }

  fun flatten() : Sum {
    val result = Sum(mapOf(), constant)
    for ((base, mult) in table) when (base) {
      is Sum -> result.combine(base.flatten(), mult)
      is Product -> result.combine(base.flatten(), mult)
      else -> result.combine(base, mult)
    }
    return result
  }
}

fun elim(e : Expression) : Expression = when (e) {
  is Numeral -> e
  is Addition -> Sum(mapOf(
      elim(e.lhs) to Numeral.ONE,
      elim(e.rhs) to Numeral.ONE))
  is Power -> Power(elim(e.base), elim(e.exponent))
  is Multiplication -> Product(mapOf(
      elim(e.lhs) to Numeral.ONE.toSum(),
      elim(e.rhs) to Numeral.ONE.toSum()))
  is Symbol -> e
  is Product -> Product(
      e.table.mapKeys { elim(it.key) },
      e.constant)
  is Sum -> Sum(
      e.table.mapKeys { elim(it.key) },
      e.constant)
}

tailrec fun simpl(e : Expression) : Expression =
    if (e == e.reduce()) e else simpl(e.reduce())

