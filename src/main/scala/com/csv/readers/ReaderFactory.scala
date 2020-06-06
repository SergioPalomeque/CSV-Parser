package com.csv.readers

import com.csv.utils.Csv

import scala.util.matching.Regex

object ReaderFactory {

  val FilePath: Regex = """(http|file)://(.*)\.([a-z]+)""".r
  val RelativePath: Regex = """(.*)\.([a-z]+)""".r

  def apply(filePath: String, delimiter: String): Either[String, Reader] =
    filePath match {
      case FilePath("http", _, _) => Right(HttpFile(filePath, delimiter, Csv))
      case FilePath("file", _, _) => Right(LocalFile(filePath, delimiter, Csv))
      case RelativePath(_, _) => Right(LocalFile(filePath, delimiter, Csv))
      case _ => Left("It was not possible to read the file")
    }

}
