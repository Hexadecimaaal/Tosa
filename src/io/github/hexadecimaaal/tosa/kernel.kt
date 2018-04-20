package io.github.hexadecimaaal.tosa

import java.math.*

sealed class Expression {
  abstract fun reduce() : Expression
  open operator fun plus(e : Expression) : Addition = Addition(this, e)
  open operator fun times(e : Expression) : Multiplication = Multiplication(this, e)
}

data class Numeral(val value : BigInteger) : Expression() {
  override fun reduce() : Numeral = this
  override fun toString() = value.toString()

  companion object {
    val ONE = Numeral(BigInteger.ONE)
    val ZERO = Numeral(BigInteger.ZERO)
    val MINUS_ONE = Numeral(BigInteger.valueOf(-1))
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
  override fun reduce() : Expression =
      if (lhs is Numeral && rhs is Numeral) Numeral(lhs.value + rhs.value)
      else Addition(lhs.reduce(), rhs.reduce())

  override fun toString() = "$lhs + $rhs"
}

data class Power(val base : Expression, val exponent : Expression) : Expression() {
  override fun toString() = "$base ^ $exponent"
  override fun reduce() : Expression =
      if (base is Numeral && exponent is Numeral) Numeral(base.value.pow(exponent.value.toInt()))
      else if (exponent == BigInteger.ONE) base
      else Power(base.reduce(), exponent.reduce())
}

data class Multiplication(val lhs : Expression, val rhs : Expression) : Expression() {
  override fun toString() = "$lhs * $rhs"
  override fun reduce() : Expression =
      if (lhs is Numeral && rhs is Numeral) Numeral(lhs.value * rhs.value)
      else Multiplication(lhs.reduce(), rhs.reduce())
}

data class Symbol(private val name : String) : Expression() {
  override fun toString() = name
  override fun reduce() : Symbol = this
}

data class Product(
    val table : Map<Expression, Sum>,
    val constant : Numeral = Numeral.ONE
) : Expression() {

  companion object {
    fun nil() : Product = Product(mapOf())
  }

  override fun reduce() : Product {
    var res = Product(mapOf(), constant)
    for ((base, exp) in table) {
      res = res.combine(base.reduce(), exp.reduce())
    }
    return elim(res).flatten().lift()
  }

  override fun toString() : String {
    var res = constant.toString()
    for ((base, exponent) in table) {
      res += " * $base"
      if (!(exponent.table.isEmpty() && exponent.constant == Numeral.ONE))
        res += " ^ $exponent"
    }
    if (res.startsWith("1 * ")) res = res.drop(4)
    return res
  }

  fun combine(e : Expression, exp : Sum = Numeral.ONE.toSum()) : Product {
    if (e is Numeral) return Product(table, constant * e)
    val result = table.toMutableMap()
    result[e] = result[e]?.combine(exp) ?: exp
    return Product(result, constant)
  }

  fun combine(pp : Product, mult : Sum = Numeral.ONE.toSum()) : Product {
    val p = pp.distrib(mult)
    val result = table.toMutableMap()
    for ((base, exp) in p.table) {
      result[base] = result[base]?.combine(exp) ?: exp
    }
    return Product(result, constant * p.constant)
  }

  fun distrib(exp : Expression) : Product =
      Product(table.mapValues {
        it.value.distrib(exp)
      }).combine(Power(constant, exp))

  fun distrib(exp : Numeral) : Product = Product(table.mapValues {
    it.value.distrib(exp)
  }, constant.power(exp))

  fun distrib(exp : Sum) : Product {
    var result = Product(mutableMapOf(), constant.power(exp.constant))

    for ((basee, mult) in exp.table) {
      for ((base, expo) in table) {
        result = result.combine(base, expo.distrib(basee).distrib(mult))
      }
      result = result.combine(constant, Sum.nil().combine(mult).combine(basee))
    }
    for ((base, expo) in table) {
      result = result.combine(base, expo.distrib(exp.constant))
    }
//    for ((base, multplicator) in exp.table)
//      result = result.combine(distrib(base).distrib(multplicator))
//    for ((base, multplicator) in exp.table)
//      result = result.combine(Power(base, base * multplicator))
    return result
  }

  fun flatten() : Product {
    var result = Product(mapOf(), constant)
    for ((base, exp) in table) result = when (base) {
      is Sum -> result.combine(base.flatten(), exp)
      is Product -> result.combine(base.flatten(), exp)
      else -> result.combine(base, exp)
    }
    return result
  }

  fun lift() : Product {
    var result = Product(mapOf(), constant)
    for ((base, exp) in table) {
      result =
          if (base is Power) result.combine(base.base, exp.combine(base.exponent))
          else result.combine(base, exp)
    }
    return result
  }
}

data class Sum(
    val table : Map<Expression, Numeral>,
    val constant : Numeral = Numeral.ZERO
) : Expression() {

  companion object {
    fun nil() : Sum = Sum(mapOf())
  }

  override fun reduce() : Sum {
    var res = nil()
    for ((base, mult) in table) {
      res = res.combine(base.reduce(), mult.reduce())
    }
    return elim(res).flatten().lift()
  }

  override fun toString() : String {
    var res = ""
    for ((base, mult) in table) {
      if (mult != Numeral.ONE) res += "$mult * "
      res += "$base + "
    }
    res += constant.toString()
    if (res.endsWith(" + 0")) res = res.dropLast(4)
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
      }).combine(mult, constant)

  fun distrib(mult : Numeral) : Sum =
      Sum(table.mapValues {
        it.value * mult
      }, constant.times(mult))

  fun distrib(mult : Sum) : Sum {
    var result = Sum(mutableMapOf(), mult.constant * constant)
    for ((base, multplicator) in mult.table)
      result = result.combine(distrib(base).distrib(multplicator))
    for ((base, multplicator) in mult.table)
      result = result.combine(base * multplicator * constant)
    return result
  }

  fun flatten() : Sum {
    var result = nil()
    for ((base, mult) in table) result = when (base) {
      is Sum -> result.combine(base.flatten(), mult)
      is Product -> result.combine(base.flatten(), mult)
      else -> result.combine(base, mult)
    }
    return result
  }

  private fun nil() = Sum(mapOf(), constant)

  fun lift() : Sum {
    var result = nil()
    for ((base, mult) in table) result =
        if (base is Product) result.combine(Product(base.table), mult * base.constant)
        else result.combine(base, mult)
    return result
  }
}

fun elim(e : Sum) : Sum = Sum(
    e.table.mapKeys { elim(it.key) },
    e.constant)

fun elim(e : Product) : Product = Product(
    e.table.mapKeys { elim(it.key) },
    e.constant)

fun elim(e : Expression) : Expression = when (e) {
  is Numeral -> e
  is Addition -> Sum.nil()
      .combine(elim(e.lhs))
      .combine(elim(e.rhs))
      .flatten()
  is Power -> Power(elim(e.base), elim(e.exponent))
  is Multiplication -> Product.nil()
      .combine(elim(e.lhs))
      .combine(elim(e.rhs))
      .flatten()
  is Symbol -> e
  is Product -> Product(
      e.table.mapKeys { elim(it.key) },
      e.constant)
      .flatten()
  is Sum -> Sum(
      e.table.mapKeys { elim(it.key) },
      e.constant)
      .flatten()
}

tailrec fun simpl(expr : Expression) : Expression {
  val e = elim(expr)
  return if (e == e.reduce()) e
  else simpl(e.reduce())
}


