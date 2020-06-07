package com.csv.processors

import org.scalatest.FunSpec
import org.scalatest.Matchers._

import scala.util.Try

class ProcessorSpec extends FunSpec {

  describe(classOf[Processor[List[(Int, String)], Unit]].getSimpleName) {

    it("should receive a string iterator and parser every line") {

      val quote = '\''.toString
      val inputSeparator = "\\|"

      val process = new Processor[List[(Int, String)], Unit] {
        override def header: Boolean = true
        override def separator: String = inputSeparator
        override def quotes: String = quote
        def lineParser(line: List[String]): Either[String, List[(Int, String)]] = {
          val r = separator + "(?=([^" + quotes + "]|" + quotes + "[^" + quotes + "]*" + quotes + ")*$)"
          val cols = line.mkString(" ").split(r, -1)
          val result = cols.map(_.trim).zipWithIndex.map(_.swap).toList

          Right(result)
        }
        def writeLine(line: List[(Int, String)]): Try[Unit] = {
          Try(println(line.mkString(" ")))
        }
      }

      val lines2 = Iterator("a|asaa|", "sds'd|s|s'dsd", "dssdsd|dsds")
      process.run(lines2, List.empty[String]).right.get shouldBe 0

    }

    it("should receive a string iterator and parser every line2") {

      val quote = '\''.toString
      val inputSeparator = "\\|"

      val process = new Processor[List[(Int, String)], Unit] {
        override def header: Boolean = true
        override def separator: String = inputSeparator
        override def quotes: String = quote
        def lineParser(line: List[String]): Either[String, List[(Int, String)]] = {
          val r = separator + "(?=([^" + quotes + "]|" + quotes + "[^" + quotes + "]*" + quotes + ")*$)"
          val cols = line.mkString(" ").split(r, -1)
          val result = cols.map(_.trim).zipWithIndex.map(_.swap).toList

          Right(result)
        }
        def writeLine(line: List[(Int, String)]): Try[Unit] = {
          Try(println(line.mkString(" ")))
        }
      }

      val lines2 = Iterator("a|asaa|", "sds'd|s|s'dsd", "dssdsd|dsds")
      process.run(lines2, List.empty[String]).right.get shouldBe 0

    }

  }

}
