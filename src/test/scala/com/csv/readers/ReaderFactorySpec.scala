package com.csv.readers

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class ReaderFactorySpec extends FunSpec {

  describe("ReaderFactory object") {

    it("should return the right class to read a csv file") {

      val relativeFilePath = ReaderFactory("relativePath.csv", "|")

      relativeFilePath.right.get shouldBe a[LocalFile]
      relativeFilePath.right.get.filePath shouldBe "relativePath.csv"
      relativeFilePath.right.get.delimiter shouldBe "|"

      val httpPath = ReaderFactory("http://dominitio.com/file.csv", ",")
      httpPath.right.get shouldBe a[HttpFile]
      httpPath.right.get.filePath shouldBe "http://dominitio.com/file.csv"
      httpPath.right.get.delimiter shouldBe ","

      val absolutePath = ReaderFactory("file:///tmp/file.csv", ",")
      absolutePath.right.get shouldBe a[LocalFile]
      absolutePath.right.get.filePath shouldBe "file:///tmp/file.csv"
      absolutePath.right.get.delimiter shouldBe ","

    }

  }

}
