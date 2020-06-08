package com.csv


import com.csv.configuration.Configuration
import com.csv.processors.Processor
import com.csv.readers.ReaderFactory

import scala.util.Try

/*
 * TODO: For all the code application, it would be good to add more comments. This is better
 *  for anyone who needs to change and to understand the code.
 */

object App extends App {
  /*
   * TODO: to add a logger to show how the application is doing or to show
   *  different kind of errors
   */

  if (args.length < 1) {
    println("At least the file path is required.")
    println(Configuration.message)
    sys.exit(1)
  }

  val config = Configuration.readArgs(args)

  val filePath = config.filePath
  val inputSeparator = config.inputSeparator
  val inputQuote = config.inputQuote
  val inputDelimiter = config.inputDelimiter
  val inputHeader = config.inputHeader

  val csv = for {
    fileType <- ReaderFactory(filePath, inputSeparator).right
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

  process.run(csv.right.get, inputHeader, inputQuote, inputDelimiter) match {
    case Right(_) => sys.exit(0)
    case Left(error) =>
      println(s"Error to parser CSV: $error")
      sys.exit(1)
  }

}
