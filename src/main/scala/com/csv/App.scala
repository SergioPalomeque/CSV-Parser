package com.csv


import com.csv.readers.ReaderFactory


object App extends App {

  val filePath = "test.csv"
  val delimiter = "\\r\\n"

  def printLine(line: Seq[String]): Unit = {
    println("line: " + line.mkString("|"))
    val cols = line.mkString.split(",(?=([^\"]|\"[^\"]*\")*$)", -1)
    val result = cols.map(_.trim).zipWithIndex.map(_.swap)
    println(result.mkString)
  }


  val csv = for {
    fileType <- ReaderFactory.apply(filePath, delimiter).right
    csv <- fileType.readCsv.right
  } yield csv

  if (csv.isLeft) {
    println(s"Error: ${csv.left.get}")
    sys.exit(1)
  }

  csv.right.get.foldLeft[(Seq[String], String)](Nil, "") {
    case ((accumulatedLines, accumulatedString), newLine) => {
      val isInAnOpenString = accumulatedString.nonEmpty
      val lineHasOddQuotes =  newLine.count(_ == '"') % 2 == 1
      (isInAnOpenString, lineHasOddQuotes) match {
        case (true, true) => {
          printLine(accumulatedLines :+ (accumulatedString + newLine))
          Nil -> ""
        }
        case (true, false) => accumulatedLines -> (accumulatedString + newLine)
        case (false, true) => accumulatedLines -> newLine
        case (false, false) => {
          printLine(accumulatedLines :+ newLine)
          Nil -> ""
        }
      }
    }
  }._1

}
