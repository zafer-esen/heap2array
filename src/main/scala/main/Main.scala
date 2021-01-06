package main

import ap.parameters.{Param, ParserSettings}
import ap.CmdlMain.NullStream
import ap.parser.IFormula

object Heap2Array {
  val version = "unstable build"

  def main(args: Array[String]) {
    val (settings, inputs) =
      try {
        GlobalSettings.fromArguments(args, GlobalSettings.DEFAULT)
      } catch {
        case e: Throwable => {
          println(e.getMessage)
          println
          printUsage
          println
          return
        }
      }

    if (Param.VERSION(settings)) {
      println(version)
      return
    }

    if (Param.FULL_HELP(settings)) {
      println(version)
      println
      printUsage
      println
      return
    }

    if (Param.QUIET(settings))
      Console setErr NullStream

    if (inputs.isEmpty) {
      Console.err.println("No inputs given, exiting")
      printUsage
      println
      return
    }

    for ((filename, ind) <- inputs zipWithIndex) try {
      val input : java.io.Reader = new java.io.BufferedReader (
        new java.io.FileReader(new java.io.File (filename)))

      val parser = SMTParser2InputAbsy(ParserSettings.DEFAULT)

      val (_, _, signature) = parser(input)
      val assertions : Seq[IFormula] = parser.extractAssertions(input)
      val funDefs = parser.functionDefs

      if (Param.PRINT_SMT_FILE(settings) != "") {
        println
        val outNamePrefix = if (inputs.size > 1) ind.toString else ""
        println("Saving the output of " + filename + " in SMT format to " +
          outNamePrefix + Param.PRINT_SMT_FILE(settings) + " ...")
        val out = new java.io.FileOutputStream(outNamePrefix +
          Param.PRINT_SMT_FILE(settings))
        Console.withOut(out) { Lineariser(assertions, funDefs, signature, "") }
        out.close
      } else {
        Lineariser(assertions, funDefs, signature, "")
      }
    } catch {
      case e : Throwable => {
        println("ERROR: " + e.getMessage)
        e.printStackTrace
      }
    }
  }

  def printUsage = {
    println("Usage: heap2array <option>* <inputfile>*")
    println
    printOptions
  }

  def printOptions = {
    println("Standard options:")
    println(" [+-]logo                  Print logo and elapsed time              (default: +)")
    println(" [+-]fullHelp              Print detailed help and exit             (default: -)")
    println(" [+-]version               Print version and exit                   (default: -)")
    println(" [+-]quiet                 Suppress all output to stderr            (default: -)")
    println(" [+-]assert                Enable runtime assertions                (default: -)")
    println(" -out=filename             Output the problem in SMT-LIB format     (default: \"\")")
  }
}