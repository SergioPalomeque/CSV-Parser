package com.csv.configuration

case class AppArgs(filePath: String,
                   inputSeparator: String,
                   inputQuote: String,
                   inputDelimiter: String,
                   inputHeader: Boolean)
object AppArgs {
  def empty = new AppArgs(
    "",
    "\\r\\n",
    "\"",
    ",",
    false)
}

object Configuration {
  /*
   * TODO: Improve the function to make sure filePath value is a required field
   *  and the other ones are optionals. Return an error according with
   *
   * TODO: Unit test for this object are missing
   */
  def readArgs(args: Array[String]): AppArgs = {
    val argsInstance = args.sliding(2, 1).toList.foldLeft(AppArgs.empty) {
      case (accumulatedArgs, currentArgs) => currentArgs match {
        case Array("--filePath", filePath) =>
          accumulatedArgs.copy(filePath = filePath)
        case Array("--inputDelimiter", inputDelimiter) =>
          accumulatedArgs.copy(inputDelimiter = inputDelimiter)
        case Array("--inputQuote", inputQuote) =>
          accumulatedArgs.copy(inputQuote = inputQuote)
        case Array("--inputSeparator", inputSeparator) =>
          accumulatedArgs.copy(inputSeparator = inputSeparator)
        case Array("--inputHeader", inputHeader) =>
          accumulatedArgs.copy(inputHeader = inputHeader.toBoolean)
      }
    }
    argsInstance
  }

  def message: String = {
    """
      |--filePath: Csv file path. Protocols types: http|file. Required
      |--inputDelimiter: How fields are divide each other in a line. Default: ","
      |--inputQuote: Quoting string for fields. Default: "\"
      |--inputSeparator: How lines are separate in the file. Default: "\\n"
      |--inputHeader: If Csv file has header or not. Default: "false"
    """.stripMargin
  }

}
