package com.langrsoft.pos

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import CheckoutJsonSupport._

import scala.collection.mutable.ListBuffer

trait CheckoutRoutes {
  var checkouts: ListBuffer[Checkout] = ListBuffer[Checkout]()
  val itemDatabase: Inventory

  def nextId() = { checkouts.length + 1 }

  def routes() : Route = {
    concat(pathPrefix("checkouts") {
      concat(
        path(Segment / "items") {
          checkoutId => { post { postItem(checkoutId) } }
        },
        path("clear") {
          clearAllCheckouts()
        },
        post { postCheckout() },
        get { getCheckout() }
      )
    },
    path("allCheckouts") {
      get { getAllCheckouts() }
    })
  }

  private def postItem(checkoutId: String) = {
    val checkout: Option[Checkout] = checkouts.toList.find(checkout => { checkout.id == checkoutId })
    if (checkout.isEmpty)
      complete(StatusCodes.NotFound, s"invalid checkout id: ${checkoutId}")
    else {
      entity(as[String]) { upc =>
        val item = itemDatabase.retrieveItem(upc)
        if (item == null)
          complete(StatusCodes.NotFound, s"invalid upc: ${upc}")
        else
          complete(StatusCodes.Accepted, item)
      }
    }
  }

  private def clearAllCheckouts() = {
    checkouts.clear()
    complete(StatusCodes.Accepted)
  }

  private def getAllCheckouts(): Route = {
    complete(checkouts.toList)
  }

  private def getCheckout() = {
    parameters('id.as[String]) { id => {
      val checkout: Option[Checkout] = checkouts.toList.find(checkout => checkout.id == id)
      complete(checkout.get)
    }}
  }

  private def postCheckout() = {
    val checkout = Checkout(nextId().toString(), "", List())
    checkout.memberId = s"member #${checkout.id}"
    checkouts += checkout
    complete(StatusCodes.Created, checkout.id)
  }
}

object CheckoutRoutesImpl extends CheckoutRoutes {
  val itemDatabase = new Inventory()
}
//  val stuff: concurrent.Future[String] = Unmarshal(entity).to[String]
