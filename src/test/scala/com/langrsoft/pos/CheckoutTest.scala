package com.langrsoft.pos

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{BeforeAndAfter, FunSpec, ShouldMatchers}
import spray.json._
import CheckoutJsonSupport._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._

class CheckoutTest extends FunSpec
    with ShouldMatchers
    with BeforeAndAfter
    with ScalatestRouteTest
    with MockitoSugar {
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

  describe("checkouts") {
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
      postItemResolvingToPrice("11", BigDecimal("3.00"))
      postItemResolvingToPrice("12", BigDecimal("4.00"))

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
      postItemResolvingToPrice("11", BigDecimal(10.00))
      postItemResolvingToPrice("22", BigDecimal(20.00))

      Get(s"/checkouts/${id1}/total") ~> testRoutes ~> check {
        responseAs[String] shouldEqual "27.00"
      }
    }

    it("applies member discount but not to exempt items") {
      postMemberWithDiscount(BigDecimal(0.1))
      postItemResolvingToPrice("11", BigDecimal(10.00))
      postExemptItemResolvingToPrice("22", BigDecimal(20.00))

      Get(s"/checkouts/${id1}/total") ~> testRoutes ~> check {
        responseAs[String] shouldEqual "29.00"
      }
    }

    it("provides 0 total for discounted items when no member scanned") {
      postItemResolvingToPrice("11", BigDecimal(10.00))
      postExemptItemResolvingToPrice("22", BigDecimal(20.00))

      Get(s"/checkouts/${id1}/total") ~> testRoutes ~> check {
        responseAs[String] shouldEqual "30.00"
      }
    }
  }

  private def postMemberWithDiscount(discount: BigDecimal) = {
    when(mockMemberDatabase.memberLookup("719-287-4335"))
      .thenReturn(Some(Member("42", "719-287-4335", "X", discount)))
    Post(s"/checkouts/${id1}/member", "719-287-4335") ~> testRoutes ~> check {}
  }

  private def postItemResolvingToPrice(upc: String, price: BigDecimal, isExemptFromDiscount: Boolean = false) = {
    when(mockItemDatabase.retrieveItem(upc))
      .thenReturn(Some(Item(upc, upc, "", price, isExemptFromDiscount)))
    Post(s"/checkouts/${id1}/items", upc) ~> testRoutes ~> check {}
  }

  private def postExemptItemResolvingToPrice(upc: String, price: BigDecimal) = {
    postItemResolvingToPrice(upc, price, true)
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
