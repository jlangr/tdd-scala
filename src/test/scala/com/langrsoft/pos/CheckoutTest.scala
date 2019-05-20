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
        val checkout: Checkout = responseAs[String].parseJson.convertTo[Checkout]
        checkout.memberId shouldEqual("member #1")
      }
    }

    it("returns all created checkouts") {
      val id2 = postCheckout

      Get(s"/allCheckouts") ~> testRoutes ~> check {
        status.isSuccess() shouldBe(true)
        val checkoutsJson = jsArrayToArrayOf[Checkout]
        checkoutsJson.map(_.memberId) shouldEqual(Seq(s"member #${id1}", s"member #${id2}"))
      }
    }
  }

  // TODO async at server?

  // TODO change GET checkout from query param

  describe("items") {
    it("returns created line item on post") {
      when(mockItemDatabase.retrieveItem("444")).thenReturn(Item("1", "444", "Eggs", BigDecimal(4.44)))

      Post(s"/checkouts/${id1}/items", "444") ~> testRoutes ~> check {
        responseAs[String].parseJson.convertTo[Item] shouldEqual(Item("1", "444", "Eggs", BigDecimal(4.44)))
      }
    }

    it("attaches items to checkout on post") {
      when(mockItemDatabase.retrieveItem("333")).thenReturn(Item("1", "333", "Milk", BigDecimal(2.79)))
      when(mockItemDatabase.retrieveItem("444")).thenReturn(Item("2", "444", "Eggs", BigDecimal(4.44)))

      Post(s"/checkouts/${id1}/items", "333") ~> testRoutes ~> check {}
      Post(s"/checkouts/${id1}/items", "444") ~> testRoutes ~> check {}

      Get(s"/checkouts?id=${id1}") ~> testRoutes ~> check {
        val checkout = responseAs[String].parseJson.convertTo[Checkout]
        checkout.items shouldEqual List(
          Item("1", "333", "Milk", BigDecimal(2.79)),
          Item("2", "444", "Eggs", BigDecimal(4.44)))
      }
    }

    it("returns error when scanned item not in database") {
      when(mockItemDatabase.retrieveItem("444")).thenReturn(null)

      Post(s"/checkouts/${id1}/items", "444") ~> testRoutes ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[String] shouldEqual "invalid upc: 444"
      }
    }

    it("returns error when checkout not found") {
      Post(s"/checkouts/999/items", "444") ~> Route.seal(TestCheckoutRoutes.routes()) ~> check {
        status shouldEqual (StatusCodes.NotFound)
        responseAs[String] shouldEqual "invalid checkout id: 999"
      }
    }
  }

  describe("member scan") {
    it("attaches member ID to checkout") {
      when(mockMemberDatabase.memberLookup("719-287-4335")).thenReturn(Member("42", "719-287-4335", "Jeff Languid", BigDecimal(0.1)))

      Post(s"/checkouts/${id1}/member", "719-287-4335") ~> testRoutes ~> check {}

      Get(s"/checkouts?id=${id1}") ~> testRoutes ~> check {
        val checkout = responseAs[String].parseJson.convertTo[Checkout]
        checkout.memberId shouldEqual "42"
      }
    }
  }

  private def jsArrayToArrayOf[T :JsonReader] = {
    responseAs[String].parseJson.asInstanceOf[JsArray].elements.map(_.convertTo[T])
  }

  private def postCheckout = {
    Post("/checkouts", "") ~> testRoutes ~> check { responseAs[String] }
  }
}
