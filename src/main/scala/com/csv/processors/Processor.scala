package com.csv.processors

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}


trait Processor[A, B] {
  def header: Boolean
  def separator: String
  def quotes: String
  def lineParser(line: List[String]): Either[String, A]
  def writeLine(line: A): Try[B]

  private def processLine(lines: List[String]): Try[Unit] = {
    for {
      lineParsed <- lineParser(lines).fold(left => Failure(new Exception(left)), right => Success(right))
      result <- writeLine(lineParsed)
    } yield result
  }

  @tailrec
  final def run(lines: Iterator[String], accumulatedString: List[String]): Either[String, Int] = {
    if (lines.hasNext) {
      val newLine = lines.next()
      val isInAnOpenString = accumulatedString.nonEmpty
      val lineHasOddQuotes =  newLine.sliding(quotes.length).count(window => window == quotes) % 2 == 1
      (isInAnOpenString, lineHasOddQuotes) match {
        case (true, true) =>
          processLine(accumulatedString ::: List(newLine)) match {
            case Success(_) => run(lines, List.empty[String])
            case Failure(error) => Left(error.getMessage)
          }
        case (true, false) => run(lines, accumulatedString ::: List(newLine))
        case (false, true) => run(lines, List(newLine))
        case (false, false) =>
          processLine(List(newLine)) match {
            case Success(_) => run(lines, List.empty[String])
            case Failure(error) => Left(error.getMessage)
          }
      }
    }
    else Right(0)
  }

}

