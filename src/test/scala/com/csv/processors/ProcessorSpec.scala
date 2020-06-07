package com.csv.processors

import org.mockito.Mockito.{spy, times, verify}
import org.scalatest.FunSpec
import org.scalatest.Matchers._
import org.scalatest.mockito.MockitoSugar

import scala.util.{Failure, Success, Try}

class ProcessorSpec extends FunSpec with MockitoSugar {

  describe(classOf[Processor[Unit]].getSimpleName) {

    it("should receive a string iterator and parser every line without error") {

      val header = false
      val quotes = "\'"
      val delimiter = "\\|"

      val process = new Processor[Unit] {
        def writeLine(line: List[(Int, String)]): Try[Unit] = {
          Success()
        }
      }

      val lines2 = Iterator("a|asaa|", "sds'd|s|s'dsd", "dssdsd|dsds")
      process.run(lines2,  header, delimiter, quotes).right.get shouldBe 0

    }

    it("should stop to parser the csv file when writeLine method get an exception") {

      val delimiter = "\\r\\n"
      val quote = "\""
      val header = false

      val process = new Processor[Unit] {
        def writeLine(line: List[(Int, String)]): Try[Unit] = {
          Failure(new Exception("Error to write a line"))
        }
      }

      val lines2 = Iterator("a|asaa|", "sds'd|s|s'dsd", "dssdsd|dsds")
      process.run(lines2, header, quote, delimiter).left.get shouldBe "Error to write a line"

    }

    it("should stop to parser the csv file when lineParser method get an exception") {

      val delimiter = "\\|"
      val quote = "\""
      val header = false

      val process = new Processor[Unit] {
        override protected def lineParser(line: List[String], separator: String, quotes: String): Either[String, List[(Int, String)]] = {
          Left("Error to parser a line")
        }
        def writeLine(line: List[(Int, String)]): Try[Unit] = {
          Success()
        }
      }

      val lines2 = Iterator("a|b|c")
      process.run(lines2, header, delimiter, quote).left.get shouldBe "Error to parser a line"

    }

    it("should parser the file with the separator and quotes length bigger than 1") {
      val header = false
      val quotes = "ZZ"
      val delimiter = "TT"


      val process = spy(new Processor[Unit] {
        def writeLine(line: List[(Int, String)]): Try[Unit] = {
          Success()
        }
      })

      val lines2 = Iterator("aTTbTT", "dTTeTTf", "aTTbTThello ZZhow TT are", "youZZ")
      process.run(lines2, header, quotes, delimiter)

      verify(process, times(1)).writeLine(List((0,"a"), (1,"b"), (2,"")))
      verify(process, times(1)).writeLine(List((0,"d"), (1,"e"), (2,"f")))
      verify(process, times(1)).writeLine(List((0,"a"), (1,"b"), (2,"hello ZZhow TT are youZZ")))

    }

  }

}
