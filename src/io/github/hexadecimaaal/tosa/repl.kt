package io.github.hexadecimaaal.tosa

import java.io.InputStream
import java.io.OutputStream
import java.io.PrintWriter

class REPL(
    inputStream : InputStream,
    outputStream : OutputStream,
    private val hint : String,
    sync : Boolean = true
) {
  private val reader = inputStream.reader()
  private val writer = PrintWriter(outputStream, sync)
  fun run() {
    writer.printf(hint)
    reader.forEachLine {
      try {
        writer.println("${simpl(parse(it), builtin)}\n")
      } catch (e : ParseException) {
        writer.println(e.message)
      }
      writer.printf(hint)
    }
  }

  fun close() {
    reader.close()
    writer.close()
  }
}

fun main(args : Array<String>) {
  val r = REPL(System.`in`, System.out, ">>>")
  r.run()
  r.close()
}
