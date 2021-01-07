package heap2array

import ap.parameters.{Param, ParserSettings}
import ap.CmdlMain.NullStream
import heap2array.SingleHeapParser.MultipleHeapsException

object Main {
  val version = "unstable build"

  import Heap2ArrayParams._
  def main(args: Array[String]) {
    val (settings, inputs) =
      try {
        GlobalSettings.fromArguments(args, GlobalSettings.DEFAULT)
      } catch {
        case e: Throwable =>
          println(e.getMessage)
          println
          printUsage
          println
          return
      }

    if (VERSION(settings)) {
      println(version)
      return
    }

    if (QUIET(settings))
      Console setErr NullStream

    if (inputs.isEmpty) {
      Console.err.println("No inputs given, exiting")
      printUsage
      println
      return
    }

    for ((filename, ind) <- inputs zipWithIndex) try {
      val input: java.io.Reader = new java.io.BufferedReader(
        new java.io.FileReader(new java.io.File(filename)))

      //var parser = SingleHeapParser(ParserSettings.DEFAULT)

      def parseAndPrint (parser : Heap2ArrayParser){
        if (OUT(settings) != "") {
          println
          val outNamePrefix = if (inputs.size > 1) ind.toString else ""
          println("Saving the output of " + filename + " in SMT format to " +
            outNamePrefix + OUT(settings) + " ...")
          val out = new java.io.FileOutputStream(outNamePrefix +
            OUT(settings))
          Console.withOut(out) {
            parser(input)
          }
          out.close()
        } else
          parser(input)
      }

      if(EXT(settings))
        parseAndPrint(SMTParser2InputAbsy(ParserSettings.DEFAULT))
      else {
        try {
          parseAndPrint(SingleHeapParser(ParserSettings.DEFAULT))
        }
        catch {
          case MultipleHeapsException =>
            parseAndPrint(SMTParser2InputAbsy(ParserSettings.DEFAULT))
          case e: Throwable => throw e
        }
      }
    } catch {
      case e : Throwable =>
        println("ERROR: " + e.getMessage)
        e.printStackTrace()
    }
  }

  def printUsage {
    println("Usage: heap2array <option>* <inputfile>*")
    println
    printOptions
  }

  def printOptions {
    println("Options:")
    println(" [+-]version               Print version and exit                   (default: -)")
    println(" [+-]quiet                 Suppress all output to stderr            (default: -)")
    println(" [+-]ext                   Encodes extensionality and supports      (default: +)")
    println("                           multiple heap declarations, but slower             ")
    println(" -out=filename             Output the problem in SMT-LIB format     (default: \"\")")
  }
}
