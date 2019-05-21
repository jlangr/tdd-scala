package com.langrsoft.pos

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import CheckoutJsonSupport._

import scala.collection.mutable.ListBuffer

trait CheckoutRoutes {
  var checkouts: ListBuffer[Checkout] = ListBuffer[Checkout]()
  val itemDatabase: Inventory
  val memberDatabase: MemberDatabase

  def nextId() = { checkouts.length + 1 }

  def routes() : Route = {
    concat(pathPrefix("checkouts") {
      concat(
        path(Segment / "items") {
          checkoutId => { post { postItem(checkoutId) } }
        },
        path(Segment / "member") {
          checkoutId => { post { postMember(checkoutId) } }
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

  private def postMember(checkoutId: String) = {
    findCheckout(checkoutId) match {
      case c if c.isEmpty =>
        complete(StatusCodes.NotFound, s"invalid checkout id: ${checkoutId}")
      case retrievedCheckout =>
        entity(as[String]) { phoneNumber =>
          memberDatabase.memberLookup(phoneNumber) match {
            case m if m.isEmpty =>
              complete(StatusCodes.NotFound, s"phone number not found: 719-287-4335")
            case member =>
              val checkout: Checkout = retrievedCheckout.get
              checkout.member = member
              complete(StatusCodes.Accepted)
          }
        }
    }
  }

  private def findCheckout(checkoutId: String) = {
    checkouts.toList.find(checkout => { checkout.id == checkoutId })
  }

  private def postItem(checkoutId: String) = {
    findCheckout(checkoutId) match {
      case c if c.isEmpty =>
        complete(StatusCodes.NotFound, s"invalid checkout id: ${checkoutId}")
      case retrievedCheckout =>
        entity(as[String]) { upc =>
          itemDatabase.retrieveItem(upc) match {
            case i if i.isEmpty =>
              complete(StatusCodes.NotFound, s"invalid upc: ${upc}")
            case item =>
              val checkout: Checkout = retrievedCheckout.get
              checkout.items = List.concat(checkout.items, List(item.get))
              complete(StatusCodes.Accepted, item.get)
          }
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
    parameters('id.as[String]) { id => complete(findCheckout(id).get)}
  }

  private def postCheckout() = {
    val checkout = Checkout(nextId().toString(), List(), None)
    checkouts += checkout
    complete(StatusCodes.Created, checkout.id)
  }
}

object CheckoutRoutesImpl extends CheckoutRoutes {
  val itemDatabase = new Inventory()
  val memberDatabase = new MemberDatabase()
}
//  val stuff: concurrent.Future[String] = Unmarshal(entity).to[String]
