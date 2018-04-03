package io.github.hexadecimaaal.tosa

import java.io.InputStream
import java.io.OutputStream
import java.io.PrintWriter

class REPL(
    inputStream : InputStream,
    outputStream : OutputStream,
    private val hint : String,
    sync : Boolean
) {
  private val reader = inputStream.reader()
  private val writer = PrintWriter(outputStream, sync)
  fun run() {
    writer.printf(hint)
    reader.forEachLine {
      writer.println("${simpl(parse(it))}\n")
      writer.printf(hint)
    }
  }

  fun close() {
    reader.close()
    writer.close()
  }
}

fun main(args : Array<String>) {
  val r = REPL(System.`in`, System.out, ">>>", true)
  r.run()
  r.close()
}
