package io.github.hexadecimaaal.tosa

interface Expression {
  fun reduce(context : Map<Symbol, Expression>) : Expression
}

val builtin : Map<Symbol, Expression> = mapOf(
    Symbol("five") to Numeral.fromInt(5)
)

data class Symbol(private val name : String) : Expression {
  override fun toString() = name
  override fun reduce(context : Map<Symbol, Expression>) : Expression =
      context.getOrDefault(this, this)
}

data class Function(val binder : Symbol, val body : Expression) : Expression {
  override fun reduce(context : Map<Symbol, Expression>) : Expression =
      body.reduce(context)
}

data class Application(val func : Function, val value : Expression) : Expression {
  override fun reduce(context : Map<Symbol, Expression>) : Expression =
      func.body.reduce(context + Pair(func.binder, value))
}
