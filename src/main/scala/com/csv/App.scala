package com.csv


import com.csv.processors.Processor
import com.csv.readers.ReaderFactory

import scala.util.{Failure, Success, Try}


object App extends App {

  val filePath = "test.csv"
  //val filePath = "C:\\Users\\X247289\\Desktop\\Data8277.csv"
  val inputDelimiter = "\\r\\n"
  val inputQuote = '\"'.toString
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

  val process = new Processor[List[(Int, String)], Unit] {
    override def header: Boolean = inputHeader
    override def separator: String = inputSeparator
    override def quotes: String = inputQuote
    def lineParser(line: List[String]): Either[String, List[(Int, String)]] = {
      val r = separator + "(?=([^" + quotes + "]|" + quotes + "[^" + quotes + "]*" + quotes + ")*$)"

      Try {
        val cols = line.mkString(" ").split(r, -1)
        val result = cols.map(_.trim).zipWithIndex.map(_.swap).toList
        result
      } match {
        case Success(fields) => Right(fields)
        case Failure(exception) => Left(exception.getMessage)
      }
    }
    def writeLine(line: List[(Int, String)]): Try[Unit] = {
      Try(println(line.mkString(" ")))
    }
  }

  process.run(csv.right.get, List.empty[String])

}
