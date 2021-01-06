package main

import ap.parameters.{Param, Settings}

object GlobalSettings {

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
          Param.VERSION.set(settings, value)
        case Opt("logo", value) =>
          Param.LOGO.set(settings, value)
        case Opt("fullHelp", value) =>
          Param.FULL_HELP.set(settings, value)
        case Opt("quiet", value) =>
          Param.QUIET.set(settings, value)
        case ValueOpt("out", value) =>
          Param.PRINT_SMT_FILE.set(settings, value)
        case Opt("assert", value) =>
          Param.ASSERTIONS.set(settings, value)
        case Opt(_, _) =>
          throw new UnknownArgumentException(arg)
        case _ => { inputs += arg; settings }
      }
    (settings, inputs)
  }

  val allParams =
    List(Param.VERSION, Param.LOGO, Param.FULL_HELP,
      Param.QUIET, /*Param.STDIN,*/ Param.ASSERTIONS, Param.PRINT_SMT_FILE)

  val DEFAULT =
    new GlobalSettings (scala.collection.immutable.HashMap[Param, Any]())
}

class GlobalSettings(_paramMap : Map[Param, Any])
  extends Settings[GlobalSettings](_paramMap) {
  protected val allParams = GlobalSettings.allParams

  protected def setParams(paramMap : Map[Param, Any]) =
    new GlobalSettings(paramMap)
}