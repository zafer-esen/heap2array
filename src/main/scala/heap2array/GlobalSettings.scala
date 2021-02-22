package heap2array

import ap.parameters.{Param, Settings}

object Heap2ArrayParams {
  case object VERSION extends Param {
    type Value = Boolean
    val defau : Boolean = false
  }
  case object QUIET extends Param {
    type Value = Boolean
    val defau : Boolean = false
  }
  case object EXT extends Param {
    type Value = Boolean
    val defau : Boolean = false
  }
  case object OUT extends Param {
    type Value = String
    val defau : String = ""
  }
}

object GlobalSettings {
  import Heap2ArrayParams._
  import ap.util.CmdlParser._

  def fromArguments(args : Seq[String]) : (GlobalSettings, Seq[String]) =
    fromArguments(args, DEFAULT)

  def fromArguments(args : Seq[String],
                    initSettings : GlobalSettings) : (GlobalSettings, Seq[String]) = {
    var settings = initSettings
    val inputs = new scala.collection.mutable.ArrayBuffer[String]

    for (arg <- args)
      settings = arg match {
        case Opt("version", value) =>
          VERSION.set(settings, value)
        case Opt("quiet", value) =>
          QUIET.set(settings, value)
        case ValueOpt("out", value) =>
          OUT.set(settings, value)
        case Opt("ext", value) =>
          EXT.set(settings, value)
        case Opt(_, _) =>
          throw new UnknownArgumentException(arg)
        case _ => inputs += arg; settings
      }
    (settings, inputs)
  }

  val allParams =
    List(VERSION, QUIET, OUT, EXT)

  val DEFAULT =
    new GlobalSettings (scala.collection.immutable.HashMap[Param, Any]())
}

class GlobalSettings(_paramMap : Map[Param, Any])
  extends Settings[GlobalSettings](_paramMap) {
  protected val allParams: List[Param] = GlobalSettings.allParams

  protected def setParams(paramMap : Map[Param, Any]) =
    new GlobalSettings(paramMap)
}