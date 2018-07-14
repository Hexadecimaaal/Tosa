package io.github.hexadecimaaal.tosa

import java.math.*

fun toMath(expr : Expression) = if (expr is MathExpression) expr else Pure(expr)

sealed class MathExpression : Expression {
  abstract override fun reduce(context : Map<Symbol, Expression>) : MathExpression
  open fun clear() : MathExpression = this.reduce(mapOf())
  fun simplify() : MathExpression = this.reduce(mapOf()).clear()
  open operator fun plus(e : MathExpression) : Addition = Addition(this, e)
  open operator fun times(e : MathExpression) : Multiplication = Multiplication(this, e)

}

data class Numeral(val value : BigInteger) : MathExpression() {
  override fun reduce(context : Map<Symbol, Expression>) : Numeral = this
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

data class Addition(val lhs : MathExpression, val rhs : MathExpression) : MathExpression() {
  override fun reduce(context : Map<Symbol, Expression>) : MathExpression =
      if (lhs is Numeral && rhs is Numeral) Numeral(lhs.value + rhs.value)
      else Addition(lhs.simplify(), rhs.simplify())

  override fun toString() = "$lhs + $rhs"
}

data class Power(val base : MathExpression, val exponent : MathExpression) : MathExpression() {
  override fun toString() = when (base.clear()) {
    is Numeral, is Pure -> base.toString()
    is Addition, is Multiplication,
    is Product, is Sum, is Power -> "($base)"
  } + when (exponent.clear()) {
    Numeral.ONE -> ""
    is Numeral, is Pure, is Power -> " ^ $exponent"
    is Addition, is Multiplication, is Product, is Sum -> " ^ ($exponent)"
  }

  override fun reduce(context : Map<Symbol, Expression>) : MathExpression =
      if (base is Numeral && exponent is Numeral) Numeral(base.value.pow(exponent.value.toInt()))
      else if (exponent == Numeral.ONE) base
//      else if (
//          exponent == Numeral.ZERO &&
//          base != Numeral.ZERO
//      ) Numeral.ONE
      else Power(base.simplify(), exponent.simplify())
}

data class Pure(val inner : Expression) : MathExpression() {
  override fun reduce(context : Map<Symbol, Expression>) : MathExpression = Pure(inner.reduce(context))
}

data class Multiplication(val lhs : MathExpression, val rhs : MathExpression) : MathExpression() {
  override fun toString() = when (lhs.clear()) {
    Numeral.ONE -> ""
    is Numeral, is Pure,
    is Multiplication, is Power,
    is Product -> "$lhs * "
    is Addition, is Sum -> "($lhs) * "
  } + when (rhs.clear()) {
    is Numeral, is Pure,
    is Power -> rhs.toString()
    is Multiplication, is Addition, is Sum,
    is Product -> "($rhs)"
  }

  override fun reduce(context : Map<Symbol, Expression>) : MathExpression =
      if (lhs is Numeral && rhs is Numeral) Numeral(lhs.value * rhs.value)
      else Multiplication(lhs.simplify(), rhs.simplify())
}

data class Product(
    val table : Map<MathExpression, Sum>,
    val constant : Numeral = Numeral.ONE
) : MathExpression() {

  companion object {
    fun nil() : Product = Product(mapOf())
  }

  override fun clear() : MathExpression =
      if (table.isEmpty()) constant
      else if (
          constant == Numeral.ONE &&
          table.size == 1
      )
        Power(table.keys.first(), table.values.first())
      else this

  override fun reduce(context : Map<Symbol, Expression>) : MathExpression {
    if (constant == Numeral.ZERO)
      return constant
    var res = Product(mapOf(), constant)
    for ((base, exp) in table) {
      res = res.combine(base.reduce(context), exp.reduce(context))
    }
    return elim(res).flatten().lift()
  }

  override fun toString() : String {
    var res = constant.toString()
    for ((base, exponent) in table) {
      res += " * ${Power(base.clear(), exponent.clear())}"
    }
    if (res.startsWith("1 * ")) res = res.drop(4)
    return res
  }

  fun combine(e : MathExpression, exp : Sum = Numeral.ONE.toSum()) : Product {
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

  fun distrib(exp : MathExpression) : Product =
      Product(table.mapValues {
        it.value.distrib(exp)
      }).combine(Power(constant, exp))

  fun distrib(exp : Numeral) : Product = Product(table.mapValues {
    it.value.distrib(exp)
  }, constant.power(exp))

  fun distrib(exp : Sum) : Product {
    var result = Product(mutableMapOf(), constant.power(exp.constant))

    for ((baseExp, mult) in exp.table) {
      for ((base, expo) in table) {
        result = result.combine(base, expo.distrib(baseExp).distrib(mult))
      }
      result = result.combine(constant, Sum.nil().combine(mult).combine(baseExp))
    }
    for ((base, expo) in table) {
      result = result.combine(base, expo.distrib(exp.constant))
    }
    return result
  }

  fun flatten() : Product {
    var result = Product(mapOf(), constant)
    for ((base, exp) in table) result = when (base) {
      is Product -> result.combine(base.flatten(), exp)
      is Power -> result.combine(base.base, exp.distrib(base.exponent))
      else -> result.combine(base, exp)
    }
    return result
  }

  private fun lift() : Product {
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
    val table : Map<MathExpression, Numeral>,
    val constant : Numeral = Numeral.ZERO
) : MathExpression() {

  companion object {
    fun nil() : Sum = Sum(mapOf())
  }

  override fun clear() : MathExpression =
      if (table.isEmpty()) constant
      else if (
          constant == Numeral.ZERO &&
          table.size == 1)
        Multiplication(table.keys.first(), table.values.first())
      else this

  override fun reduce(context : Map<Symbol, Expression>) : Sum {
    var res = nil()
    for ((base, mult) in table) {
      val x = base.simplify()
      res = if (x is Numeral) res.combine(x.toSum(), mult.reduce(context))
      else res.combine(x, mult.reduce(context))
    }
    return elim(res).flatten().lift()
  }

  override fun toString() : String {
    var res = ""
    for ((base, mult) in table)
      res += "${Multiplication(mult, base)} + "
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

  fun combine(e : MathExpression, mult : Numeral = Numeral.ONE) : Sum {
    if (e is Numeral) return Sum(table, constant + e)
    val result = table.toMutableMap()
    result[e] = result[e]?.plus(mult) ?: mult
    return Sum(result, constant)
  }

  fun distrib(mult : MathExpression) : Sum =
      Sum(table.mapKeys {
        it.key * mult
      }).combine(mult, constant)

  fun distrib(mult : Numeral) : Sum =
      Sum(table.mapValues {
        it.value * mult
      }, constant.times(mult))

  fun distrib(mult : Sum) : Sum {
    var result = Sum(mutableMapOf(), mult.constant * constant)
    for ((base, multiplicator) in mult.table)
      result = result.combine(distrib(base).distrib(multiplicator))
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

  private fun lift() : Sum {
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

fun elim(e : MathExpression) : MathExpression = when (e) {
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
  is Pure -> e
  is Product -> Product(
      e.table.mapKeys { elim(it.key) },
      e.constant)
      .flatten()
  is Sum -> Sum(
      e.table.mapKeys { elim(it.key) },
      e.constant)
      .flatten()
}

tailrec fun simpl(expr : MathExpression) : MathExpression {
  val e = elim(expr).simplify()
  return if (e == expr) e
  else simpl(e)
}


