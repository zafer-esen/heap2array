package heap2array

import ap.Signature
import ap.parameters.ParserSettings
import ap.parser.SMTParser2InputAbsy._
import ap.parser._
import ap.parser.smtlib.Absyn._
import ap.parser.smtlib._

object SingleHeapParser {

  import ap.parser.Parser2InputAbsy._

  protected[heap2array] object MultipleHeapsException extends Exception(
    "Multiple heap declarations detected, switching to smarter parser...")

  def apply(settings : ParserSettings) =
    new SingleHeapParser (settings)

  /**
   * Parse starting at an arbitrarily specified entry point
   */
  private def parseWithEntry[T](input : java.io.Reader,
                                entry : parser => T) : T = {
    val l = new Yylex(new CRRemover2 (input))
    val p = new parser(l)

    try { entry(p) } catch {
      case e : Exception =>
        throw new ParseException(
          "At line " + String.valueOf(l.line_num()) +
            ", near \"" + l.buff() + "\" :" +
            "     " + e.getMessage)
    }
  }
}

////////////////////////////////////////////////////////////////////////////////

class SingleHeapParser(settings : ParserSettings) extends Heap2ArrayParser {

  var heapDeclCount = 0

  import SingleHeapParser._
  import ap.parser.Parser2InputAbsy.ParseException

  /** Implicit conversion so that we can get a Scala-like iterator from a
   * a Java list */
  import scala.collection.JavaConversions.{asScalaBuffer, asScalaIterator}

  type GrammarExpression = Term

  //////////////////////////////////////////////////////////////////////////////

  private object ExitException extends Exception("SMT-LIB interpreter terminated")

  //////////////////////////////////////////////////////////////////////////////

  def apply(input: java.io.Reader) :
    (IFormula, List[IInterpolantSpec], Signature) = {
    def entry(parser: smtlib.parser) = {
      val parseTree = parser.pScriptC
      parseTree match {
        case parseTree: Script => parseTree
        case _ => throw new ParseException("Input is not an SMT-LIB 2 file")
      }
    }

    apply(parseWithEntry(input, entry))
    (null, null, null)
  }

  //////////////////////////////////////////////////////////////////////////////

  def processIncrementally(input: java.io.Reader): Unit = {
    val l = new Yylex(new SMTCommandTerminator(input))
    val p = new parser(l) {
      override def commandHook(cmd: Command): Boolean = {
        try {
          apply(cmd)
        } catch {
          case e: Exception =>
        }
        false
      }

      override def report_error(message: String, info: Object): Unit = {
        Console.err.println(message)
      }
    }

    try {
      p.pScriptC
    } catch {
      case ExitException =>
        // normal exit
        input.close
      case e: Exception =>
        //        e.printStackTrace
        throw new ParseException(
          "At line " + String.valueOf(l.line_num()) +
            ", near \"" + l.buff() + "\" :" +
            "     " + e.getMessage)
    }
  }

  /**
   * Parse an SMT-LIB script of the form
   * <code>(ignore expression)</code>.
   */
  def parseIgnoreCommand(input: java.io.Reader) = {
    def entry(parser: smtlib.parser) = {
      val parseTree = parser.pScriptC
      parseTree match {
        case script: Script
          if script.listcommand_.size == 1 =>
          script.listcommand_.head match {
            case cmd: IgnoreCommand => cmd.term_
            case _ =>
              throw new ParseException(
                "Input is not of the form (ignore expression)")
          }
        case _ => throw new ParseException(
          "Input is not of the form (ignore expression)")
      }
    }

    parseWithEntry(input, entry)
  }

  protected def push {
    println("(push 1)")
  }

  protected def pop {
    println("(pop 1)")
  }

  //////////////////////////////////////////////////////////////////////////////

  private val printer = new PrettyPrinterNonStatic

  //////////////////////////////////////////////////////////////////////////////

  private def apply(script: Script): Unit =
    for (cmd <- script.listcommand_) apply(cmd)

  private def apply(cmd: Command): Unit = {
    cmd match {

      //////////////////////////////////////////////////////////////////////////

      case cmd: HeapDeclCommand =>
        heapDeclCount += 1

        if (heapDeclCount > 1) throw MultipleHeapsException

        val heapName = asString(cmd.identifier_1)
        val addrName = asString(cmd.identifier_2)
        val objName = asString(cmd.identifier_3)

        println(";" + "=" * 79)
        println("; Encoding of " + heapName + " sorts and operations")
        println(";" + "-" * 79)
        println("(define-sort " + addrName + "() Int)")

        // print any ADT declarations that are part of declare-heap
        if (cmd.listpolysortc_ nonEmpty) {
          println("(declare-datatypes (" + (printer print cmd.listpolysortc_) + ")\n" +
            " " * 19 + "(" +
            (for (decl <- cmd.listmaybepardatadecl_) yield
              printer print decl).mkString("\n" + " " * 19) +
            "))")
        }

        // print heap ADTs and operations
        println("(declare-datatypes ((AllocRes" + heapName + " 0) (" + heapName + " 0))\n" +
          "                   (((AllocRes" + heapName + "   (new" + heapName + " " + heapName + ") (new" + addrName + " " + addrName + ")))\n" +
          "                    ((" + heapName + "Ctor (" + heapName + "Size Int)\n" +
          "                               (" + heapName + "Contents (Array " + addrName + " " + objName + "))))))\n" +
          "(define-fun null" + addrName + "  () " + addrName + " 0)\n" +
          "(define-fun def" + objName + "    () " + objName + " " + (printer print cmd.term_) + ")\n" +
          "(define-fun valid     ((h " + heapName + ") (p " + addrName + ")) Bool\n" +
          "  (and (>= (" + heapName + "Size h) p) (> p 0)))\n" +
          "(declare-const allDef" + objName + " (Array " + addrName + " " + objName + "))\n" +
          "(define-fun empty" + heapName + " () " + heapName + " (" + heapName + "Ctor 0 allDef" + objName + "))\n" +
          "(define-fun read ((h " + heapName + ") (p " + addrName + ")) " + objName + "\n" +
          "  (ite (valid h p)\n" +
          "       (select (" + heapName + "Contents h) p)\n" +
          "       def" + objName + "))\n" +
          "(define-fun write ((h " + heapName + ") (p " + addrName + ") (o " + objName + ")) " + heapName + "\n" +
          "  (ite (valid h p)\n" +
          "       (" + heapName + "Ctor (" + heapName + "Size h) (store (" + heapName + "Contents h) p o))\n" +
          "       h))\n" +
          "(define-fun alloc   ((h " + heapName + ") (o " + objName + ")) AllocRes" + heapName + "\n" +
          "  (AllocRes" + heapName + " (" + heapName + "Ctor (+ 1 (" + heapName + "Size h))\n" +
          "                    (store (" + heapName + "Contents h) (+ 1 (" + heapName + "Size h)) o))\n" +
          "          (+ 1 (" + heapName + "Size h))))\n" +
          "(define-fun " + heapName + "-eq     ((h1 " + heapName + ") (h2 " + heapName + ")) Bool\n" +
          "  (forall ((p " + addrName + "))\n" +
          "          (and (= (valid h1 p) (valid h2 p))\n" +
          "               (= (read h1 p) (read h2 p)))))")

        println(";" + "=" * 79)

      /*case cmd : PushCommand => // todo: are these two needed?
      for (_ <- 0 until cmd.numeral_.toInt)
        push

    case cmd : PopCommand =>
      for (_ <- 0 until cmd.numeral_.toInt)
        pop

    case cmd : AssertCommand =>
      val f = asFormula(translateTerm(cmd.term_, -1))
      print("(assert ")
      SMTLineariser(FunPredSubstVisitor(f, heapFunMap.toMap, heapPredMap.toMap))
      println(")")*/

      //////////////////////////////////////////////////////////////////////////

      case _: EmptyCommand => // command to be ignored

      //////////////////////////////////////////////////////////////////////////

      case _ => println(printer print cmd)
    }
  }
}