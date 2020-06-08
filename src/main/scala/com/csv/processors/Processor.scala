package com.csv.processors

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}



trait Processor[A] {
  /*
   * TODO: To add a implicit Type Class in this method in order to make it more generic
   *  to save the parsed line. This Type class can have different methods implemented to
   *  save the parsed line into any database or another file.
   *
   * TODO: Another thing to add would be the header field if it exist. In this way, it would
   *  be possible, for example, to create a json with field name and their values.
   */
  def writeLine(line: List[(Int, String)]): Try[A]

  protected def lineParser(line: List[String], quotes: String, delimiter: String): Either[String, List[(Int, String)]] = {
    val r = delimiter + "(?=([^" + quotes + "]|" + quotes + "[^" + quotes + "]*" + quotes + ")*$)"

    Try {
      val cols = line.mkString(" ").split(r, -1)
      val result = cols.map(_.trim).zipWithIndex.map(_.swap).toList
      result
    } match {
      case Success(fields) => Right(fields)
      case Failure(exception) => Left(exception.getMessage)
    }
  }

  private def processLine(lines: List[String], quotes: String, separator: String): Try[Unit] = {
    for {
      lineParsed <- lineParser(lines, quotes, separator)
        .fold(left => Failure(new Exception(left)), right => Success(right))
      _ <- writeLine(lineParsed)
    } yield ()
  }

  /*
   * TODO: To change the returned value type Either[String, Int] by Either[ErrorProcess, Code].
   *  This change would offer a better user experience.
   */
  def run(lines: Iterator[String],
          header: Boolean, quotes: String, delimiter: String): Either[String, Int] = {
    @tailrec
    def parseCsv(lines: Iterator[String], accumulatedString: List[String]): Either[String, Int] = {
      if (lines.hasNext) {
        val newLine = lines.next()
        val isInAnOpenString = accumulatedString.nonEmpty
        val lineHasOddQuotes =  newLine.sliding(quotes.length).count(window => window == quotes) % 2 == 1
        (isInAnOpenString, lineHasOddQuotes) match {
          case (true, true) =>
            processLine(accumulatedString ::: List(newLine), quotes, delimiter) match {
              case Success(_) => parseCsv(lines, List.empty[String])
              case Failure(error) => Left(error.getMessage)
            }
          case (true, false) => parseCsv(lines, accumulatedString ::: List(newLine))
          case (false, true) => parseCsv(lines, List(newLine))
          case (false, false) =>
            processLine(List(newLine), quotes, delimiter) match {
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

