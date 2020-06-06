package com.csv.readers

import com.csv.utils.Csv
import org.scalatest.FunSpec
import org.scalatest.Matchers._

import scala.util.{Failure, Try}

class ReaderSpec extends FunSpec {

  describe(classOf[Reader].getSimpleName) {

    it("should read the first line in the iterator") {

      val utilCsvMock = new Csv {
        override def readLocalFile(url: String, delimiter: String): Try[Iterator[String]] = {
          Try(Iterator("a"))
        }

        override def readFileFromHttp(url: String, delimiter: String): Try[Iterator[String]] = {
          Try(Iterator("b"))
        }
      }

      LocalFile("file", "delimiter", utilCsvMock).readCsv.right.get.next() shouldBe "a"
      HttpFile("file", "delimiter", utilCsvMock).readCsv.right.get.next() shouldBe "b"
    }

    it("should get a String type with the error exception") {
      val utilCsvMock = new Csv {
        override def readLocalFile(url: String, delimiter: String): Try[Iterator[String]] = {
          Failure(new Exception("Exception for local file"))
        }

        override def readFileFromHttp(url: String, delimiter: String): Try[Iterator[String]] = {
          Failure(new Exception("Exception for Http file"))
        }
      }

      LocalFile("file", "delimiter", utilCsvMock).readCsv.left.get shouldBe "Exception for local file"
      HttpFile("file", "delimiter", utilCsvMock).readCsv.left.get shouldBe "Exception for Http file"

    }

  }
}
