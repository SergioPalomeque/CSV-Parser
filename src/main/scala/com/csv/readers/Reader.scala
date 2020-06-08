package com.csv.readers

import com.csv.utils.Csv

import scala.util.{Failure, Success}

sealed trait Reader {
  def util: Csv
  def filePath: String
  def delimiter: String
  /*
  * TODO: in general, to do a better parser of failures that can arise when a file is read.
  *  In this way, this API could provide a better experience for clients and for user
  *  executing the process.
  *
  *  Example: To change Either[String, Iterator[String]] -> Either[ErrorMessage, Iterator[String]]
  */
  def readCsv: Either[String, Iterator[String]]
}



case class LocalFile(filePath: String, delimiter: String, util: Csv) extends Reader {

  override def readCsv: Either[String, Iterator[String]] = {
    util.readLocalFile(filePath,delimiter) match {
      case Success(iterator) => Right(iterator)
      case Failure(e) => Left(e.getMessage)
    }
  }
}

/*
 * TODO: to remove the unnecessary 'Csv with'
 */
case class HttpFile(filePath: String, delimiter: String, util: Csv) extends Reader with Csv {

  override def readCsv: Either[String, Iterator[String]] = {
    util.readFileFromHttp(filePath, delimiter) match {
      case Success(iterator) => Right(iterator)
      case Failure(e) => Left(e.getMessage)
    }
  }
}
