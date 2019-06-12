package com.langrsoft.util

import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}

class Portfolio6Test extends FunSpec with BeforeAndAfter with Matchers {
  var portfolio: Portfolio6Data = null

  before {
    portfolio = Portfolio6Data()
  }

  describe("a portfolio") {
    it("is empty when created") {
      Portfolio6.isEmpty(portfolio) shouldBe true
    }

    it("is not empty after purchase") {
      portfolio = Portfolio6.buy(portfolio, "BAYN", 10)

      Portfolio6.isEmpty(portfolio) shouldBe false
    }

    it("has symbol count 0 when created") {
      Portfolio6.count(portfolio) shouldBe 0
    }

    it("has symbol count 1 after purchase") {
      portfolio = Portfolio6.buy(portfolio, "BAYN", 10)

      Portfolio6.count(portfolio) shouldBe 1
    }

    it("increments symbol with purchase of unique symbol") {
      portfolio = Portfolio6.buy(portfolio, "BAYN", 10)
      portfolio = Portfolio6.buy(portfolio, "IBM", 20)

      Portfolio6.count(portfolio) shouldBe 2
    }

    it("does not increment symbol on purchase of same symbol") {
      portfolio = Portfolio6.buy(portfolio, "BAYN", 10)
      portfolio = Portfolio6.buy(portfolio, "BAYN", 20)

      Portfolio6.count(portfolio) shouldBe 1
    }

    it("returns 0 for shares of symbol not purchased") {
      Portfolio6.sharesOf(portfolio, "BAYN") shouldBe 0
    }

    it("returns shares of symbol purchased") {
      portfolio = Portfolio6.buy(portfolio, "BAYN", 20)
      portfolio = Portfolio6.buy(portfolio, "IBM", 5)

      Portfolio6.sharesOf(portfolio, "BAYN") shouldBe 20
    }

    it("adds shares for same-symbol purchase") {
      portfolio = Portfolio6.buy(portfolio, "BAYN", 20)
      portfolio = Portfolio6.buy(portfolio, "BAYN", 5)

      Portfolio6.sharesOf(portfolio, "BAYN") shouldBe 25
    }

    it("reduces shares on sell") {
      portfolio = Portfolio6.buy(portfolio, "BAYN", 20)
      portfolio = Portfolio6.sell(portfolio, "BAYN", 5)

      Portfolio6.sharesOf(portfolio, "BAYN") shouldBe 15
    }

    it("is empty after sell all") {
      portfolio = Portfolio6.buy(portfolio, "BAYN", 20)
      portfolio = Portfolio6.sell(portfolio, "BAYN", 20)

      Portfolio6.isEmpty(portfolio) shouldBe true
    }

    it("throws when selling more than held") {
      an [IllegalArgumentException] should be thrownBy(
        Portfolio6.sell(portfolio, "BAYN", 20))
    }
  }
}
