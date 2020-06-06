package com.csv.readers

import com.csv.utils.Csv

import scala.util.{Failure, Success}

sealed trait Reader {
  def util: Csv
  def filePath: String
  def delimiter: String
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

case class HttpFile(filePath: String, delimiter: String, util: Csv) extends Reader with Csv {

  override def readCsv: Either[String, Iterator[String]] = {
    util.readFileFromHttp(filePath, delimiter) match {
      case Success(iterator) => Right(iterator)
      case Failure(e) => Left(e.getMessage)
    }
  }
}
