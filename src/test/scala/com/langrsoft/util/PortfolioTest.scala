package com.langrsoft.util

import org.mockito.{IdiomaticMockito, MockitoSugar}
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}
import org.mockito.Mockito._

//object TestPortfolio extends Portfolio with IdiomaticMockito {
//  override val stockService = mock[StockService]
//}

class PortfolioTest extends FunSpec
  with Matchers with BeforeAndAfter
  with IdiomaticMockito
{
  val BayerPrice = 19
  val IbmPrice = 19
  var portfolio: Portfolio = _
  val stockService = mock[StockService]

  before {
    portfolio = new Portfolio(stockService)
//    portfolio.auditor = mock[Auditor]
  }

  describe("a portfolio") {
    it("is empty when created") {
      portfolio.isEmpty shouldBe true
    }

    it("has size 0 when created") {
      portfolio.size shouldBe 0
    }

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


    describe("value") {

      it("is zero when created") {
        portfolio.value shouldBe 0
      }

      it("is share value after purchase single share") {
        val stockService = new StockService {
          override def price(symbol: String): Integer = BayerPrice
        }

        portfolio.purchase("BAYN", 1)

        portfolio.valueByHand(stockService) shouldBe BayerPrice
      }
//
//      it("multiples price by shares") {
//        portfolio.stockService = new StockService {
//          override def price(symbol: String): Integer = BayerPrice
//        }
//
//        portfolio.purchase("BAYN", 10)
//
//        portfolio.value shouldBe BayerPrice * 10
//      }
//
      it("accumulates prices for all symbols") {
//        portfolio.stockService = new StockService {
//          def price(symbol: String): Integer = {
//            symbol match {
//              case "BAYN" => BayerPrice
//              case "IBM" => IbmPrice
//            }
//          }
//        }
//
//        portfolio.purchase("BAYN", 10)
//        portfolio.purchase("IBM", 20)
//
//        portfolio.value shouldBe BayerPrice * 10 + IbmPrice * 20
      }
    }

    describe("value using mockito") {
      it("accumulates prices for all symbols") {
        val stockService = mock[StockService]
        val portfolio = new Portfolio(stockService)
        when(stockService.price("BAYN")) thenReturn(BayerPrice)
        when(stockService.price("IBM")) thenReturn(IbmPrice)

        portfolio.purchase("BAYN", 10)
        portfolio.purchase("IBM", 20)

        portfolio.value shouldBe BayerPrice * 10 + IbmPrice * 20
      }
    }

//    describe("transaction audits") {
//      it("audits on purchase") {
//        portfolio.purchase("BAYN", 10)
//
//        // if one matcher used all args must use matchers
//        verify(portfolio.auditor).
//          audit(stringEq("Purchased 10 shares of BAYN"), any[java.util.Date])
//      }
//    }
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

