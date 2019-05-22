package com.langrsoft.util

import org.scalatest.{FunSpec, ShouldMatchers}

import scala.collection.mutable.ListBuffer

class BasicsTest extends FunSpec with ShouldMatchers {
  describe("some javascript fundamentals") {
    ignore("supports basic math") {
      4 * 8 shouldEqual 0 // fix this
    }

    ignore("appends an item to an array using push") {
      val numbers = ListBuffer(12, 1, 1, 1, 2, 1, 3)

      numbers += 1

      numbers should contain theSameElementsInOrderAs
        Seq(/* ? */)
    }

    ignore("doubles each element an array of numbers ") {
      val numbers = Seq(2, 5, 10, 105)

      val result = numbers map identity // fix this!

      result should contain theSameElementsInOrderAs
        Seq(4, 10, 20, 210)
    }

    ignore("handles interesting float-point number results") {
      val result = 0.1 + 0.2

      result should be (0) // what? Fix this. See docs if needed
    }
  }
}
