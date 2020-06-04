package com.csv

import java.nio.charset.CodingErrorAction
import java.util.Scanner

import scala.collection.JavaConverters._
import scala.io.{Codec, Source}


object App extends App {

  implicit val codec: Codec = Codec("UTF-8")
  codec.onMalformedInput(CodingErrorAction.REPLACE)
  codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

  def printLine(line: Seq[String]): Unit = {
    println("line: " + line.mkString("|"))
    val cols = line.mkString.split(",(?=([^\"]|\"[^\"]*\")*$)", -1)
    val result = cols.map(_.trim).zipWithIndex.map(_.swap)
    println(result.mkString)
  }

  val file = Source.fromFile("test.csv").bufferedReader()

  val input = new Scanner(file)
    .useDelimiter("\\r\\n")
    .asScala

  input.foldLeft[(Seq[String], String)](Nil, "") {
    case ((accumulatedLines, accumulatedString), newLine) => {
      val isInAnOpenString = accumulatedString.nonEmpty
      val lineHasOddQuotes =  newLine.count(_ == '"') % 2 == 1
      (isInAnOpenString, lineHasOddQuotes) match {
        case (true, true) => {
          printLine(accumulatedLines :+ (accumulatedString + newLine))
          Nil -> ""
        }
        case (true, false) => accumulatedLines -> (accumulatedString + newLine)
        case (false, true) => accumulatedLines -> newLine
        case (false, false) => {
          printLine(accumulatedLines :+ newLine)
          Nil -> ""
        }
      }
    }
  }._1

}
