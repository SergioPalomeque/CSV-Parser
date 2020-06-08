package com.csv.utils

import java.nio.charset.CodingErrorAction
import java.util.Scanner

import scala.io.{Codec, Source}
import scala.collection.JavaConverters._
import scala.util.Try

/*
 * TODO: to test different codes file. I could not test it and I sure this could be done
 *  in a better way
 */
trait Csv {

  def readLocalFile(filePath: String, delimiter: String): Try[Iterator[String]] = {
    implicit val codec: Codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

    Try{
      val file = Source.fromFile(filePath).bufferedReader()
      new Scanner(file)
        .useDelimiter(delimiter)
        .asScala
    }
  }

  def readFileFromHttp(url: String, delimiter: String): Try[Iterator[String]] = {
    implicit val codec: Codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

    Try{
      val file = Source.fromURL(url).bufferedReader()

      new Scanner(file)
        .useDelimiter(delimiter)
        .asScala
    }
  }

}

object Csv extends Csv
