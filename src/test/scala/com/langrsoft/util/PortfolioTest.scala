package com.langrsoft.util

import org.scalatest.mock.MockitoSugar
import org.scalatest.{BeforeAndAfter, FunSpec, ShouldMatchers}
import org.mockito.Mockito._
import org.mockito.Matchers.{any, eq => stringEq }

class PortfolioTest extends FunSpec
  with ShouldMatchers with BeforeAndAfter with MockitoSugar {
  var portfolio: Portfolio = null

  before {
    portfolio = new Portfolio
    portfolio.auditor = mock[Auditor]
  }

  describe("a portfolio") {
    it("is empty when created") {
      portfolio.isEmpty shouldBe true
    }

    it("has size 0 when created") {
      portfolio.size shouldBe 0
    }

    // TODO: nesting describe / it

    it("is no longer empty after purchase") {
      portfolio.purchase("BAYN", 10)

      portfolio.isEmpty shouldBe false
    }

    it("increases size after purchase") {
      portfolio.purchase("BAYN", 10)

      portfolio.size shouldBe 1
    }

    it("increments size with each purchase") {
      portfolio.purchase("BAYN", 10)
      portfolio.purchase("IBM", 10)

      portfolio.size shouldBe 2
    }

    it("does not increment size with same symbol purchase") {
      portfolio.purchase("BAYN", 10)
      portfolio.purchase("BAYN", 10)

      portfolio.size shouldBe 1
    }

    it("returns the number of shares purchased") {
      portfolio.purchase("BAYN", 42)

      portfolio.shares("BAYN") shouldBe 42
    }

    it("returns 0 for symbol not purchased") {
      portfolio.shares("BAYN") shouldBe 0
    }

    it("adds shares for same symbol purchase") {
      portfolio.purchase("BAYN", 42)
      portfolio.purchase("BAYN", 10)

      portfolio.shares("BAYN") shouldBe 52
    }

    it("throws on purchase non positive shares") {
      an [InvalidPurchaseException] should be thrownBy
        portfolio.purchase("BAYN", 0)
    }

    val BayerPrice = 19
    val IbmPrice = 19

    describe("value") {
      it("is zero when created") {
        portfolio.value shouldBe 0
      }

      it("is share value after purchase single share") {
        portfolio.stockService = new StockService {
          override def price(symbol: String): Integer = BayerPrice
        }

        portfolio.purchase("BAYN", 1)

        portfolio.value shouldBe BayerPrice
      }

      it("multiples price by shares") {
        portfolio.stockService = new StockService {
          override def price(symbol: String): Integer = BayerPrice
        }

        portfolio.purchase("BAYN", 10)

        portfolio.value shouldBe BayerPrice * 10
      }

      it("accumulates prices for all symbols") {
        portfolio.stockService = new StockService {
          override def price(symbol: String): Integer = {
            symbol match {
              case "BAYN" => BayerPrice
              case "IBM" => IbmPrice
            }
          }
        }

        portfolio.purchase("BAYN", 10)
        portfolio.purchase("IBM", 20)

        portfolio.value shouldBe BayerPrice * 10 + IbmPrice * 20
      }
    }

    describe("value using mockito") {
      it("accumulates prices for all symbols") {
        portfolio.stockService = mock[StockService]
        when(portfolio.stockService.price("BAYN")).
          thenReturn(BayerPrice)
        when(portfolio.stockService.price("IBM")).
          thenReturn(IbmPrice)

        portfolio.purchase("BAYN", 10)
        portfolio.purchase("IBM", 20)

        portfolio.value shouldBe BayerPrice * 10 + IbmPrice * 20
      }
    }

    describe("transaction audits") {
      it("audits on purchase") {
        portfolio.purchase("BAYN", 10)

        // if one matcher used all args must use matchers
        verify(portfolio.auditor).
          audit(stringEq("Purchased 10 shares of BAYN"), any[java.util.Date])
      }
    }
  }
}


// TODO: partial classes?
// default values on arguments?
// ...
// injection techniques; polymorphism
// scalatest vs mockito
// what's the [] syntax for mockito?
// what is a case class
// date type?

