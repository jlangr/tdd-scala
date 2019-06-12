package com.langrsoft.util

import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ListBuffer

class BasicsTest extends FunSpec with Matchers {
  describe("some javascript fundamentals") {
    it("supports basic math") {
      4 * 8 shouldEqual 32
    }

    it("appends an item to an array using push") {
      val numbers = ListBuffer(12, 1, 1, 1, 2, 1, 3)

      numbers += 1

      numbers should contain theSameElementsInOrderAs
        Seq(12, 1, 1, 1, 2, 1, 3, 1)
    }

    it("doubles each element an array of numbers ") {
      val numbers = Seq(2, 5, 10, 105)

      val result = numbers map { n => n * 2 }

      result should contain theSameElementsInOrderAs
        Seq(4, 10, 20, 210)
    }

    it("handles interesting float-point number results") {
      val result = 0.1 + 0.2

      result should be (0.3 +- 0.005)
    }
  }
}
