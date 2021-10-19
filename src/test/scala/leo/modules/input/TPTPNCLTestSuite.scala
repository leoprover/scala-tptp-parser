package leo.modules.input

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

class TPTPNCLTestSuite extends AnyFunSuite {
  private[this] val files: Seq[File] = new File(getClass.getResource("/non-classical/").getPath).listFiles().toSeq

  for (file <- files) {
    test(file.getName) {
      parseAndPrint(file)
    }
  }

  private def parseAndPrint(file: File): Unit = {
    try {
      val parseResult = TPTPParser.problem(io.Source.fromFile(file))
      println(parseResult.pretty)
    } catch {
      case e: TPTPParser.TPTPParseException =>
        println(s"Parse error at ${e.line}:${e.offset}: ${e.getMessage}")
        fail(e)
    }
  }
}
