package com.csv.processors

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}



trait Processor[A] {
  def writeLine(line: List[(Int, String)]): Try[A]

  protected def lineParser(line: List[String], separator: String, quotes: String): Either[String, List[(Int, String)]] = {
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

  private def processLine(lines: List[String], separator: String, quotes: String): Try[Unit] = {
    for {
      lineParsed <- lineParser(lines, separator, quotes)
        .fold(left => Failure(new Exception(left)), right => Success(right))
      _ <- writeLine(lineParsed)
    } yield ()
  }

  def run(lines: Iterator[String],
          header: Boolean = true, separator: String = ",", quotes: String = "\""): Either[String, Int] = {
    @tailrec
    def parseCsv(lines: Iterator[String], accumulatedString: List[String]): Either[String, Int] = {
      if (lines.hasNext) {
        val newLine = lines.next()
        val isInAnOpenString = accumulatedString.nonEmpty
        val lineHasOddQuotes =  newLine.sliding(quotes.length).count(window => window == quotes) % 2 == 1
        (isInAnOpenString, lineHasOddQuotes) match {
          case (true, true) =>
            processLine(accumulatedString ::: List(newLine), separator, quotes) match {
              case Success(_) => parseCsv(lines, List.empty[String])
              case Failure(error) => Left(error.getMessage)
            }
          case (true, false) => parseCsv(lines, accumulatedString ::: List(newLine))
          case (false, true) => parseCsv(lines, List(newLine))
          case (false, false) =>
            processLine(List(newLine), separator, quotes) match {
              case Success(_) => parseCsv(lines, List.empty[String])
              case Failure(error) => Left(error.getMessage)
            }
        }
      }
      else Right(0)
    }

    parseCsv(lines,  List.empty[String])
  }

}

