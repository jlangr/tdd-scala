package com.langrsoft.pos

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.langrsoft.pos.CheckoutJsonSupport._
import org.mockito.IdiomaticMockito
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}
import spray.json._

class CheckoutTest extends FunSpec
    with Matchers
    with BeforeAndAfter
    with IdiomaticMockito
    with ScalatestRouteTest {
  val mockItemDatabase = mock[Inventory]
  val mockMemberDatabase = mock[MemberDatabase]

  object TestCheckoutRoutes extends CheckoutRoutes {
    val itemDatabase = mockItemDatabase
    val memberDatabase = mockMemberDatabase
  }

  var testRoutes: Route = Route.seal(TestCheckoutRoutes.routes())
  var id1: String = null

  before {
    Post("/checkouts/clear") ~> testRoutes ~> check {}
    id1 = postCheckout
  }

  describe("a new (posted) checkout") {
    it("returns created checkout by ID") {
      Get(s"/checkouts?id=${id1}") ~> testRoutes ~> check {
        status.isSuccess() shouldBe(true)
        convertJsonResponseTo[Checkout].id shouldEqual(id1)
      }
    }

    it("returns all created checkouts") {
      val id2 = postCheckout

      Get(s"/allCheckouts") ~> testRoutes ~> check {
        status.isSuccess() shouldBe(true)
        val checkoutsJson = jsArrayToArrayOf[Checkout]
        checkoutsJson.map(_.id) shouldEqual(Seq(id1, id2))
      }
    }
  }

  // TODO change GET checkout from query param

  describe("items") {
    it("returns created line item on post") {
      when(mockItemDatabase.retrieveItem("444")).thenReturn(Some(Item("1", "444", "Eggs", BigDecimal(4.44), false)))

      Post(s"/checkouts/${id1}/items", "444") ~> testRoutes ~> check {
        convertJsonResponseTo[Item] shouldEqual(Item("1", "444", "Eggs", BigDecimal(4.44), false))
      }
    }

    it("attaches items to checkout on post") {
      when(mockItemDatabase.retrieveItem("333")).thenReturn(Some(Item("1", "333", "Milk", BigDecimal(2.79), false)))
      when(mockItemDatabase.retrieveItem("444")).thenReturn(Some(Item("2", "444", "Eggs", BigDecimal(4.44), false)))

      Post(s"/checkouts/${id1}/items", "333") ~> testRoutes ~> check {}
      Post(s"/checkouts/${id1}/items", "444") ~> testRoutes ~> check {}

      Get(s"/checkouts?id=${id1}") ~> testRoutes ~> check {
        convertJsonResponseTo[Checkout].items shouldEqual List(
          Item("1", "333", "Milk", BigDecimal(2.79), false),
          Item("2", "444", "Eggs", BigDecimal(4.44), false))
      }
    }

    it("returns error when scanned item not in database") {
      when(mockItemDatabase.retrieveItem("444")).thenReturn(None)

      Post(s"/checkouts/${id1}/items", "444") ~> testRoutes ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[String] shouldEqual "invalid upc: 444"
      }
    }

    it("returns error when checkout not found") {
      Post(s"/checkouts/999/items", "444") ~> testRoutes ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[String] shouldEqual "invalid checkout id: 999"
      }
    }
  }

  describe("member scan") {
    it("attaches member ID to checkout") {
      when(mockMemberDatabase.memberLookup("719-287-4335"))
        .thenReturn(Some(Member("42", "719-287-4335", "Jeff Languid", BigDecimal(0.1))))

      Post(s"/checkouts/${id1}/member", "719-287-4335") ~> testRoutes ~> check {}

      Get(s"/checkouts?id=${id1}") ~> testRoutes ~> check {
        convertJsonResponseTo[Checkout].member.get.phoneNumber shouldEqual("719-287-4335")
      }
    }

    it("returns error when checkout not found") {
      Post(s"/checkouts/999/member", "whatever") ~> testRoutes ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[String] shouldEqual "invalid checkout id: 999"
      }
    }

    it("returns error when member not found") {
      when(mockMemberDatabase.memberLookup("719-287-4335"))
        .thenReturn(None)

      Post(s"/checkouts/${id1}/member", "719-287-4335") ~> testRoutes ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[String] shouldEqual "phone number not found: 719-287-4335"
      }
    }
  }

  describe("checkout total") {
    it("returns total of items scanned") {
      postItemResolvingToPrice("11", "", BigDecimal("3.00"))
      postItemResolvingToPrice("12", "", BigDecimal("4.00"))

      Get(s"/checkouts/${id1}/total") ~> testRoutes ~> check {
        responseAs[String] shouldEqual "7.00"
      }
    }

    it("returns error when checkout not found") {
      Get(s"/checkouts/999/total", "whatever") ~> testRoutes ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[String] shouldEqual "invalid checkout id: 999"
      }
    }

    it("applies member discount") {
      postMemberWithDiscount(BigDecimal(0.1))
      postItemResolvingToPrice("11", "", BigDecimal(10.00))
      postItemResolvingToPrice("22", "", BigDecimal(20.00))

      Get(s"/checkouts/${id1}/total") ~> testRoutes ~> check {
        responseAs[String] shouldEqual "27.00"
      }
    }

    it("applies member discount but not to exempt items") {
      postMemberWithDiscount(BigDecimal(0.1))
      postItemResolvingToPrice("11", "", BigDecimal(10.00))
      postExemptItemResolvingToPrice("22", "", BigDecimal(20.00))

      Get(s"/checkouts/${id1}/total") ~> testRoutes ~> check {
        responseAs[String] shouldEqual "29.00"
      }
    }

    it("provides 0 total for discounted items when no member scanned") {
      postItemResolvingToPrice("11", "", BigDecimal(10.00))
      postExemptItemResolvingToPrice("22", "", BigDecimal(20.00))

      Get(s"/checkouts/${id1}/total") ~> testRoutes ~> check {
        responseAs[String] shouldEqual "30.00"
      }
    }
  }

  describe("post total") {
    it("returns checkout with receipt and totals") {
      postItemResolvingToPrice("123", "Milk", 5.00)
      postItemResolvingToPrice("555", "Fancy eggs", 12.00)

      Post(s"/checkouts/${id1}/total") ~> testRoutes ~> check {
        val checkout = convertJsonResponseTo[Checkout]
        checkout.receipt.total shouldEqual 17.00
        checkout.receipt.totalSaved shouldEqual 0.0
        checkout.receipt.totalOfDiscountedItems shouldEqual 0.0
        checkout.receipt.lineItems
          .shouldEqual(Seq(
            "Milk                                     5.00",
            "Fancy eggs                              12.00",
            "TOTAL                                   17.00"))
      }
    }

    it("does stuff") {
      Post("/checkouts/clear") ~>
        testRoutes ~> check {
          status shouldEqual(StatusCodes.Accepted)
      }
      id1 = postCheckout
      when(mockMemberDatabase.memberLookup("719-287-4335"))
        .thenReturn(Some(Member("42", "719-287-4335", "Jeff Langr", BigDecimal(0.1))))
      Post(s"/checkouts/${id1}/member", "719-287-4335") ~> testRoutes ~> check {
        status shouldEqual(StatusCodes.Accepted)
      }
      val firstId = "1"
      val itemToReturn123 = Item(firstId, "123", "Milk", 5.00, false)
      when(mockItemDatabase.retrieveItem("123"))
        .thenReturn(Some(itemToReturn123))
      Post(s"/checkouts/${id1}/items", "123") ~> testRoutes ~> check {
        status shouldEqual(StatusCodes.Accepted)
      }
      println(s"checkout id1: ${id1}")
      val id2 = "2"
      val itemToReturn555 = Item(id2, "555", "Eggs", 2.79, false)
      when(mockItemDatabase.retrieveItem("555"))
        .thenReturn(Some(itemToReturn555))
      Post(s"/checkouts/${id1}/items", "555") ~> testRoutes ~> check {
        status shouldEqual(StatusCodes.Accepted)
      }
      Post(s"/checkouts/${id1}/total") ~> testRoutes ~> check {
        val checkout = convertJsonResponseTo[Checkout]
        checkout.receipt.total shouldEqual 7.01
        checkout.receipt.totalSaved shouldEqual 0.78
        checkout.receipt.totalOfDiscountedItems shouldEqual 7.01
        val totalOfDiscountedItems = checkout.receipt.totalOfDiscountedItems
        val lineItems = checkout.receipt.lineItems
        println(s"${totalOfDiscountedItems}")
        lineItems
          .shouldEqual(Seq(
            "Milk                                     5.00",
            "   10% mbr disc                         -0.50",
            "Eggs                                     2.79",
            "   10% mbr disc                         -0.28",
            "TOTAL                                    7.01",
            "*** You saved:                           0.78"
          ))
      }

      // not found
      Post(s"/checkouts/999/total") ~> testRoutes ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[String] shouldEqual "invalid checkout id: 999"
      }
    }
  }

  private def postMemberWithDiscount(discount: BigDecimal) = {
    when(mockMemberDatabase.memberLookup("719-287-4335"))
      .thenReturn(Some(Member("42", "719-287-4335", "X", discount)))
    Post(s"/checkouts/${id1}/member", "719-287-4335") ~> testRoutes ~> check {}
  }

  private def postItemResolvingToPrice(upc: String, description: String, price: BigDecimal, isExemptFromDiscount: Boolean = false) = {
    when(mockItemDatabase.retrieveItem(upc))
      .thenReturn(Some(Item(upc, upc, description, price, isExemptFromDiscount)))
    Post(s"/checkouts/${id1}/items", upc) ~> testRoutes ~> check {}
  }

  private def postExemptItemResolvingToPrice(upc: String, description: String, price: BigDecimal) = {
    postItemResolvingToPrice(upc, description, price, true)
  }

  private def jsArrayToArrayOf[T :JsonReader] = {
    responseAs[String].parseJson.asInstanceOf[JsArray].elements.map(_.convertTo[T])
  }

  private def postCheckout = {
    Post("/checkouts", "") ~> testRoutes ~> check { responseAs[String] }
  }

  def convertJsonResponseTo[T: JsonReader] = {
    responseAs[String].parseJson.convertTo[T]
  }
}
