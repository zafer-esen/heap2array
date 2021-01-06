package main

import ap.parameters.{Param, ParserSettings}
import ap.CmdlMain.NullStream
import ap.Signature
import ap.parser.SMTParser2InputAbsy.{SMTADT, SMTBool}
import ap.parser.{IFormula, SMTParser2InputAbsy}
import ap.theories.TheoryRegistry
import ap.types.MonoSortedPredicate

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
      val functionTypeMap = parser.functionTypeMap
      // todo: is there a better way to add invariants to the signature?
      // the way that I extended the signature looks really wrong, and it checks
      // maybe add the parser t the project and modify there?
      val extSignature = signature.updateOrder(signature.order.extendPred(
        (for ((f, typ) <- functionTypeMap if typ.result == SMTBool &&
                          {
                            typ.arguments.size != 1 ||
                              (typ.arguments.head match {
                              case _ : SMTADT => !f.name.startsWith("is-")
                              case _ => true
                            })
                          }) yield {
          MonoSortedPredicate(f.name, typ.arguments.map(t => t.toSort))
        }).toSeq
      ))
      val assertions : Seq[IFormula] = parser.extractAssertions(input)

      if (Param.PRINT_SMT_FILE(settings) != "") {
        println
        val outNamePrefix = if (inputs.size > 1) ind.toString else ""
        println("Saving the output of " + filename + " in SMT format to " +
          outNamePrefix + Param.PRINT_SMT_FILE(settings) + " ...")
        val out = new java.io.FileOutputStream(outNamePrefix +
          Param.PRINT_SMT_FILE(settings))
        Console.withOut(out) { Lineariser(assertions, functionTypeMap,
                                          extSignature, "") }
        out.close
      } else {
        Lineariser(assertions, functionTypeMap, extSignature, "")
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
    println(" [+-]version               Print version and exit                   (default: -)")
    println(" [+-]quiet                 Suppress all output to stderr            (default: -)")
    println(" [+-]assert                Enable runtime assertions                (default: -)")
    println(" -out=filename             Output the problem in SMT-LIB format     (default: \"\")")
  }
}
