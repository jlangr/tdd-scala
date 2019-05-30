package com.langrsoft.util

import com.langrsoft.util.Portfolio4._
import org.mockito.{ArgumentMatchersSugar, IdiomaticMockito}
import org.scalatest.{BeforeAndAfter, Matchers, fixture}

class Portfolio5Test extends fixture.FunSpec
  with fixture.ConfigMapFixture
  with Matchers with BeforeAndAfter
  with IdiomaticMockito
  with ArgumentMatchersSugar
{
//  type FixtureParam = PortfolioData

  val BayerSharesPurchased = 10
  val IbmSharesPurchased = 20
  val BayerPrice = 19
  val IbmPrice = 100
  val stockService: StockService = mock[StockService]

  def withOnePurchase(test: PortfolioData => Any) = {
    var holdings = PortfolioData()
    holdings = purchase(holdings, "BAYN", BayerSharesPurchased)
    test(holdings)
  }

  def withMultiplePurchases(test: PortfolioData => Any) = {
    var multipleHoldings = PortfolioData()
    multipleHoldings = purchase(multipleHoldings, "BAYN", BayerSharesPurchased)
    multipleHoldings = purchase(multipleHoldings, "IBM", IbmSharesPurchased)
    test(multipleHoldings)
  }

  describe("a portfolio") {
    /*
    it("is empty for newly created data") { _ =>
      isEmpty(PortfolioData()) shouldBe true
    }

    it("has size 0 when created") { _ =>
      uniqueSymbolCount(PortfolioData()) shouldBe 0
    }

    it("is no longer empty after purchase") { _ =>
      val portfolioData = purchase(PortfolioData(), "BAYN", 10)

      isEmpty(portfolioData) shouldBe false
    }

    it("increases size after purchase") { _ =>
      val portfolioData = purchase(PortfolioData(), "BAYN", 10)

      uniqueSymbolCount(portfolioData) shouldBe 1
    }

    it("increments size with each purchase") { _ =>
      var portfolioData = purchase(PortfolioData(), "BAYN", 10)
      portfolioData = purchase(portfolioData, "IBM", 10)

      uniqueSymbolCount(portfolioData) shouldBe 2
    }

    it("does not increment size with same symbol purchase") { _ =>
      var portfolioData = purchase(PortfolioData(), "BAYN", 10)
      portfolioData = purchase(portfolioData, "BAYN", 10)

      uniqueSymbolCount(portfolioData) shouldBe 1
    }

    it("returns the number of shares purchased") { _ =>
      val portfolioData = purchase(PortfolioData(), "BAYN", 17)

      shares(portfolioData, "BAYN") shouldBe 17
    }

    it("returns 0 for symbol not purchased") { _ =>
      shares(PortfolioData(), "BAYN") shouldBe 0
    }

    it("adds shares for same symbol purchase") { twoSymbolPort =>
      var portfolioData = purchase(PortfolioData(), "BAYN", 42)
      portfolioData = purchase(portfolioData, "BAYN", 10)

      shares(portfolioData, "BAYN") shouldBe 52
    }

    it("returns shares specific to symbol") { twoSymbolPort =>
      shares(twoSymbolPort, "BAYN") shouldBe BayerSharesPurchased
    }

    it("throws on purchase non positive shares") { _ =>
      an [InvalidPurchaseException] should be thrownBy
        purchase(PortfolioData(), "BAYN", 0)
    }
     */

    describe("a portfolio with multiple holdings") {
      describe("value") {
        it("is zero when created") { _ =>
          portfolioValue(PortfolioData(), mock[StockService]) shouldBe 0
        }

        it("returns the number of shares purchased") { configMap =>
          withOnePurchase { portfolioData =>
            shares(portfolioData, "BAYN") shouldBe BayerSharesPurchased
          }
        }

        it("accumulates prices for all symbols") { configMap =>
          withMultiplePurchases { portfolioData =>
            stockService.price("BAYN") shouldReturn BayerPrice
            stockService.price("IBM") shouldReturn IbmPrice

            portfolioValue(portfolioData, stockService)
              .shouldBe(BayerPrice * BayerSharesPurchased + IbmPrice * IbmSharesPurchased)
          }
        }
      }
    }
  }
}
