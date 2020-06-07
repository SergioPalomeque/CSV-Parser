package com.csv


import com.csv.processors.Processor
import com.csv.readers.ReaderFactory

import scala.util.Try


object App extends App {

  val filePath = "test.csv"
  //val filePath = "C:\\Users\\X247289\\Desktop\\Data8277.csv"
  val inputDelimiter = "\\r\\n"
  val inputQuote = "\""
  val inputSeparator = ","
  val inputHeader = false

  val csv = for {
    fileType <- ReaderFactory(filePath, inputDelimiter).right
    csv <- fileType.readCsv.right
  } yield csv

  if (csv.isLeft) {
    println(s"Error: ${csv.left.get}")
    sys.exit(1)
  }

  val process = new Processor[Unit] {
    def writeLine(line: List[(Int, String)]): Try[Unit] = {
      Try(println(line.mkString(" ")))
    }
  }

  process.run(csv.right.get, inputHeader, inputSeparator, inputQuote)

}
