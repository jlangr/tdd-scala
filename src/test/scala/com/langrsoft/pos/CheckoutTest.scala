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
  object TestCheckoutRoutes extends CheckoutRoutes {
    val itemDatabase = mockItemDatabase
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

  describe("items") {
    it("returns created line item on post") {
      when(mockItemDatabase.retrieveItem("444")).thenReturn(Item("1", "444", "Eggs", BigDecimal(4.44)))

      Post(s"/checkouts/${id1}/items", "444") ~> testRoutes ~> check {
        responseAs[String].parseJson.convertTo[Item] shouldEqual(Item("1", "444", "Eggs", BigDecimal(4.44)))
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

    // attaches item to checkout
  }

  private def jsArrayToArrayOf[T :JsonReader] = {
    responseAs[String].parseJson.asInstanceOf[JsArray].elements.map(_.convertTo[T])
  }

  private def postCheckout = {
    Post("/checkouts", "") ~> testRoutes ~> check { responseAs[String] }
  }
}
