package io.github.hexadecimaaal.tosa

import java.math.*

fun Expression.toMath() = this as? MathExpression ?: Pure(this)

sealed class MathExpression : Expression {
  abstract override fun reduce(context : Map<Symbol, Expression>) : MathExpression
  open operator fun plus(e : MathExpression) : MathExpression =
      toSum().plus(e)

  open operator fun times(e : MathExpression) : MathExpression =
      toProduct().times(e)

  open fun power(e : MathExpression) : MathExpression =
      Power(this, e)

  open fun flatten() : MathExpression = this

  open fun toSum() : Sum = Sum(mapOf(this to Numeral.ONE))
  open fun toProduct() : Product = Product(mapOf(this to Numeral.ONE.toSum()))
}

data class Numeral(val value : BigInteger) : MathExpression() {
  override fun reduce(context : Map<Symbol, Expression>) : Numeral = this
  override fun toString() = value.toString()

  companion object {
    val ONE = Numeral(BigInteger.ONE)
    val ZERO = Numeral(BigInteger.ZERO)
    val MINUS_ONE = Numeral(BigInteger.valueOf(-1))
    fun fromInt(i : Long) = Numeral(BigInteger.valueOf(i))
  }

  override operator fun plus(e : MathExpression) : MathExpression =
      if (e is Numeral)
        Numeral(value + e.value)
      else
        e + this

  operator fun plus(e : Numeral) : Numeral =
      Numeral(value + e.value)

  override operator fun times(e : MathExpression) : MathExpression =
      if (e is Numeral)
        Numeral(value * e.value).toProduct()
      else
        e * this

  operator fun times(e : Numeral) : Numeral =
      Numeral(value * e.value)

  fun power(e : Numeral) : Numeral =
      Numeral(value.pow(e.value.toInt()))

  override fun toSum() : Sum = Sum(mapOf(), this)
  override fun toProduct() = Product(mapOf(), this)
}

//data class Addition(val lhs : MathExpression, val rhs : MathExpression) : MathExpression() {
//  override fun reduce(context : Map<Symbol, Expression>) : MathExpression =
//      if (lhs is Numeral && rhs is Numeral) Numeral(lhs.value + rhs.value)
//      else Addition(lhs.simplify(), rhs.simplify())
//
//  override fun toString() = "$lhs + $rhs"
//}

data class Power(
    val base : MathExpression,
    val exponent : MathExpression
) : MathExpression() {
  override fun toString() = when (base) {
    is Numeral                   -> "$base"
    is Pure                      -> if (base.inner is Symbol) base.toString() else "($base)"
    is Product, is Sum, is Power -> "($base)"
  } + when (exponent) {
    Numeral.ONE                   -> ""
    is Numeral, is Pure, is Power -> " ^ $exponent"
    is Product, is Sum            -> " ^ ($exponent)"
  }

  override fun reduce(context : Map<Symbol, Expression>) : MathExpression {
    return Power(base.reduce(context), exponent.reduce(context)).flatten()
  }

  override fun toProduct() : Product = Product(mapOf(base to exponent))

  override fun flatten() : MathExpression {
    val expf = exponent.flatten()
    return if (expf == Numeral.ONE) base.flatten()
    else if (base is Numeral && exponent is Numeral)
      Numeral(base.value.pow(exponent.value.toInt()))
  //      else if (
  //          exponent == Numeral.ZERO &&
  //          base != Numeral.ZERO
  //      ) Numeral.ONE
    else if (base is Product)
      (Product(base.table.mapValues { it.value * exponent }) *
          base.ratio.power(exponent)).flatten()
    else Power(base.flatten(), expf)
  }
}

data class Pure(val inner : Expression) : MathExpression() {
  override fun reduce(context : Map<Symbol, Expression>) : MathExpression =
      inner as? MathExpression ?: Pure(inner.reduce(context))

  override fun toString() = inner.toString()
}

//data class Multiplication(val lhs : MathExpression, val rhs : MathExpression) : MathExpression() {
//  override fun toString() = when (lhs) {
//    Numeral.ONE -> ""
//    is Numeral, is Pure,
//    is Multiplication, is Power,
//    is Product -> "$lhs * "
//    is Addition, is Sum -> "($lhs) * "
//  } + when (rhs) {
//    is Numeral, is Pure,
//    is Power -> rhs.toString()
//    is Multiplication, is Addition, is Sum,
//    is Product -> "($rhs)"
//  }
//
//  override fun reduce(context : Map<Symbol, Expression>) : MathExpression =
//      if (lhs is Numeral && rhs is Numeral) Numeral(lhs.value * rhs.value)
//      else Multiplication(lhs.simplify(), rhs.simplify())
//}

data class Product(
    val table : Map<MathExpression, MathExpression>,
    val ratio : Numeral = Numeral.ONE
) : MathExpression() {

  override fun reduce(context : Map<Symbol, Expression>) : MathExpression =
      table.entries
          .fold(ratio.toProduct()) { acc, (base, exp) ->
            acc * Power(base, exp).reduce(context)
          }
          .flatten()

  override fun toString() : String =
      when {
        table.size == 1      -> "$ratio "
        ratio != Numeral.ONE -> "$ratio * "
        else                 -> ""
      } + table.entries.joinToString(" * ") { (base, exp) ->
        Power(base, exp).toString()
      }

  override operator fun times(e : MathExpression) : Product = when (e) {
    is Numeral -> Product(table, ratio * e)
    is Power   -> Product(
        table + Pair(
            e.base,
            table.getOrDefault(e.base, Numeral.ZERO) + e.exponent
        ), ratio
    )
    is Product -> e.table.entries
        .fold((ratio * e.ratio).toProduct()) { acc, (base, exp) ->
          acc * Power(base, exp)
        }
    else       -> times(Power(e, Numeral.ONE))
  }

/*  fun power(exp : MathExpression) : MathExpression =
      Product(table.mapValues {
        it.value.distrib(exp)
      }).times(Power(ratio, exp))

  fun power(exp : Numeral) : Product = Product(table.mapValues {
    it.value * exp
  }, ratio.power(exp))

  fun power(exp : Sum) : Product {
    var result = Product(mutableMapOf(), ratio.power(exp.constant))

    for ((baseExp, mult) in exp.table) {
      for ((base, expo) in table) {
        result = result.times(base, expo.distrib(baseExp).distrib(mult))
      }
      result = result.times(ratio.toProduct(), Sum.nil().plus(mult).plus(baseExp))
    }
    for ((base, expo) in table) {
      result = result.times(base, expo.distrib(exp.constant))
    }
    return result
  }*/

  override fun flatten() : MathExpression =
      if (ratio == Numeral.ZERO || table.isEmpty())
        ratio
      else if (ratio == Numeral.ONE && table.size == 1)
        Power(table.keys.first(), table.values.first()).flatten()
      else
        table.entries
            .fold(ratio.toProduct()) { acc, (base, exp) ->
              when (base) {
                Numeral.ONE -> acc
                is Sum      -> acc * Power(base.flatten(), exp.flatten())
                is Product  -> acc * Power(base.flatten(), exp.flatten())
                is Power    -> acc * Power(base.base.flatten(), (exp + base.exponent).flatten())
                else        -> acc * Power(base.flatten(), exp.flatten())
              }
            }

  override fun toProduct() : Product = this
}

data class Sum(
    val table : Map<MathExpression, Numeral>,
    val constant : Numeral = Numeral.ZERO
) : MathExpression() {

  override fun reduce(context : Map<Symbol, Expression>) : MathExpression =
      table.entries
          .fold(constant.toSum()) { acc, (base, mult) ->
            acc + base.reduce(context) * mult
          }
          .flatten()

  override fun toString() : String =
      table.entries.joinToString(" + ") { (term, ratio) ->
        "${term * ratio}"
      } + when {
        constant == Numeral.ZERO -> ""
        table.isEmpty()          -> "$constant"
        else                     -> " + $constant"
      }

//  fun plus(s : Sum) : Sum =
//      Sum(s.table.entries.fold(table) { acc, (term, ratio) ->
//        acc + Pair(term, acc.getOrDefault(term, Numeral.ZERO) + ratio)
//      }, constant.plus(s.constant))

  override operator fun plus(e : MathExpression) : Sum = when (e) {
    is Numeral -> Sum(table, constant + e)
    is Sum     -> Sum(
        e.table.entries
            .fold(table) { acc, (term, ratio) ->
              acc + Pair(term, acc.getOrDefault(term, Numeral.ZERO) + ratio)
            }, constant.plus(e.constant)
    )
    is Product -> {
      val term = Product(e.table)
      val ratio = e.ratio
      Sum(
          table + Pair(term, table.getOrDefault(term, Numeral.ZERO) + ratio)
      )
    }
    else       -> plus(e * Numeral.ONE)
  }

  override operator fun times(e : MathExpression) : Sum =
      Sum(table.mapKeys {
        it.key * e
      }).plus(e * constant)

  fun distrib(mult : Sum) : Sum =
      mult.table.entries
          .fold(Numeral.ZERO.toSum()) { acc, (term, ratio) ->
            acc + this * term * ratio
          } + this * mult.constant

  override fun flatten() : MathExpression =
      if (table.isEmpty())
        constant
      else if (constant == Numeral.ZERO && table.size == 1)
        Product(mapOf(table.keys.first() to Numeral.ONE.toSum()), table.values.first()).flatten()
      else
        table.entries
            .fold(constant.toSum()) { acc, (term, ratio) ->
              when (term) {
                is Sum     -> acc + term.flatten() * ratio
                is Product -> acc + Product(term.table).flatten() * ratio * term.ratio
                else       -> acc + term.flatten() * ratio
              }
            }

  override fun toSum() : Sum = this
}

tailrec fun simpl(expr : Expression, context : Map<Symbol, Expression>) : Expression {
  val e = expr.reduce(context)
  return if (e == expr) e
  else simpl(e, context)
}


