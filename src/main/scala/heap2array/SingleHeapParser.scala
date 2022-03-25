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

        val heap = asString(cmd.identifier_1)
        val addr = asString(cmd.identifier_2)
        val obj = asString(cmd.identifier_3)
        val addrRange = addr + "Range"
        val addrRangeStart = addrRange + "Start"
        val addrRangeSize = addrRange + "Size"
        val allocRes = "AllocRes" + heap
        val newHeap = "new" + heap
        val newAddr = "new" + addr
        val batchAllocRes = "Batch" + allocRes
        val newBatchHeap = "newBatch" + heap
        val newAddrRange = "new" + addrRange
        val heapCtor = heap + "Ctor"
        val heapSize = heap + "Size"
        val heapContents = heap + "Contents"
        val nullAddr = "null" + addr
        val emptyHeap = "empty" + heap

        println(";" + "=" * 79)
        println("; Encoding of " + heap + " sorts and operations")
        println(";" + "-" * 79)
        println("(define-sort " + addr + "() Int)")
        println("(declare-datatypes ((" + addrRange + " 0))\n" +
          "                   (((" + addrRange + " (" + addrRangeStart + " " + addr + ") (" + addrRangeSize + " Int)))))\n")


        // print any ADT declarations that are part of declare-heap
        if (cmd.listpolysortc_ nonEmpty) {
          println("(declare-datatypes (" + (printer print cmd.listpolysortc_) + ")\n" +
            " " * 19 + "(" +
            (for (decl <- cmd.listmaybepardatadecl_) yield
              printer print decl).mkString("\n" + " " * 19) +
            "))")
        }
        val defObjName = printer print cmd.term_
        // print heap ADTs and operations
        println(
          "(declare-datatypes ((" + batchAllocRes + " 0) (" + allocRes + " 0) (" + heap + " 0))\n" +
          "                   (((" + batchAllocRes + "   (" + newBatchHeap + " " + heap + ") (" + newAddrRange + " " + addrRange + ")))\n" +
          "                   ((" + allocRes + "   (" + newHeap + " " + heap + ") (" + newAddr + " " + addr + ")))\n" +
          "                    ((" + heapCtor + " (" + heapSize + " Int)\n" +
          "                               (" + heapContents + " (Array " + addr + " " + obj + "))))))\n" +

          "(define-fun " + nullAddr + "  () " + addr + " 0)\n" +

          "(define-fun valid     ((h " + heap + ") (p " + addr + ")) Bool\n" +
          "  (and (>= (" + heapSize + " h) p) (> p 0)))\n" +

          "(define-fun " + emptyHeap + " () " + heap + " (\n" +
          "  " + heap + "Ctor 0 " + "(( as const (Array " + addr + " " + obj + ")) " + defObjName + ")))\n" +

          "(define-fun read ((h " + heap + ") (p " + addr + ")) " + obj + "\n" +
          "  (ite (valid h p)\n" +
          "       (select (" + heapContents + " h) p)\n" +
          "       " + defObjName + "))\n" +

          "(define-fun write ((h " + heap + ") (p " + addr + ") (o " + obj + ")) " + heap + "\n" +
          "  (ite (valid h p)\n" +
          "       (" + heap + "Ctor (" + heap + "Size h) (store (" + heapContents + " h) p o))\n" +
          "       h))\n" +

          "(define-fun alloc   ((h " + heap + ") (o " + obj + ")) " + allocRes + "\n" +
          "  (" + allocRes + " (" + heapCtor + " (+ 1 (" + heapSize + " h))\n" +
          "                    (store (" + heapContents + " h) (+ 1 (" + heapSize + " h)) o))\n" +
          "          (+ 1 (" + heapSize + " h))))\n" // +

//          "(define-fun nth" + addrRange + "((ar " + addrRange + ") (n " + "Int" + ")) " + addr + "\n" +
//          "  (ite (and (>= n 0) (< n (" + addrRangeSize + " ar)))\n" +
//          "       (+ (" + addrRangeStart + " ar) n)" +
//          "       " + nullAddr + "))" +
//
//          " (define-fun within ((ar " + addrRange + " ) (p " + addr + ")) Bool\n" +
//          "  (and (>= p (" + addrRangeStart + " ar)) (< p (+ (" + addrRangeStart + " ar) (" + addrRangeSize + " ar)))))\n" +
//
//          "(define-fun-rec batchAllocLoop ((h " + heap + ") (o " + obj + ") (n Int) (bar " + batchAllocRes + ")) " + batchAllocRes + "\n" +
//          "  (ite (> n 0)\n"+
//          "  (batchAllocLoop (" + newHeap + " (alloc h o)) o (- n 1) (" + batchAllocRes + " (" + newHeap + " (alloc h o)) (" + newAddrRange + " bar) ))\n" +
//          "  bar))\n" +
//
//          "(define-fun batchAlloc ((h " + heap + ") (o " + obj + ") (n Int)) " + batchAllocRes + "\n" +
//          "  (batchAllocLoop h o n ("+batchAllocRes+" h ("+addrRange+" (+ 1 ("+heapSize+" h)) n))))\n"
        )
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

      //case _: SetLogicCommand => // ignore

      case _ => println(printer print cmd)
    }
  }
}