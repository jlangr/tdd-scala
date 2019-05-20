package com.langrsoft.pos

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{BeforeAndAfter, FunSpec, ShouldMatchers}
import spray.json._
import CheckoutJsonSupport._
import akka.http.scaladsl.server.Route
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._

class CheckoutTest extends FunSpec
    with ShouldMatchers
    with BeforeAndAfter
    with ScalatestRouteTest
    with MockitoSugar {
  val route = CheckoutRoutesImpl.routes()

  before {
    Post("/checkouts/clear") ~> route ~> check {}
  }

  describe("checkouts") {
    it("returns created checkout by ID") {
      val id = postCheckout

      Get(s"/checkouts?id=${id}") ~> route ~> check {
        status.isSuccess() shouldBe(true)

        val checkout: Checkout = responseAs[String].parseJson.convertTo[Checkout]
        checkout.memberId shouldEqual("member #1")
      }
    }

    it("returns all created checkouts") {
      val id1 = postCheckout
      val id2 = postCheckout

      Get(s"/allCheckouts") ~> route ~> check {
        status.isSuccess() shouldBe(true)
        val checkoutsJson = jsArrayToArrayOf[Checkout]
        checkoutsJson.map(_.memberId) shouldEqual(Seq(s"member #${id1}", s"member #${id2}"))
      }
    }
  }

  // todo: ADD id to Item
  describe("items") {
    it("returns created line item on post") {
      val mockItemDatabase = mock[Inventory]
      object TestCheckoutRoutes extends CheckoutRoutes {
        val itemDatabase = mockItemDatabase
      }
      val upc = "444"
      when(mockItemDatabase.retrieveItem("444")).thenReturn(Item("444", "Eggs", BigDecimal(4.44)))
      val id = postCheckout
      Post(s"/checkouts/${id}/items", upc) ~> Route.seal(TestCheckoutRoutes.routes()) ~> check {
        responseAs[String].parseJson.convertTo[Item] shouldEqual(Item(upc, "Eggs", BigDecimal(4.44)))
      }
    }
  }

  private def jsArrayToArrayOf[T :JsonReader] = {
    responseAs[String].parseJson.asInstanceOf[JsArray].elements.map(_.convertTo[T])
  }

  private def postCheckout = {
    Post("/checkouts", "") ~> route ~> check { responseAs[String] }
  }
}
