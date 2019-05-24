package com.langrsoft.pos

import java.math.MathContext

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import CheckoutJsonSupport._

import scala.collection.mutable.ListBuffer
import scala.math.BigDecimal.RoundingMode

// This trait does a couple or more core things. How might you split it up?
trait CheckoutRoutes {
  val checkouts: ListBuffer[Checkout] = ListBuffer[Checkout]()
  val itemDatabase: Inventory
  val memberDatabase: MemberDatabase

  def nextId() = { checkouts.length + 1 }

  def routes() : Route = {
    concat(pathPrefix("checkouts") {
      concat(
        path(Segment / "items") {
          checkoutId => post { postItem(checkoutId) }
        },
        path(Segment / "member") {
          checkoutId => post { postMember(checkoutId) }
        },
        path(Segment / "total") {
          checkoutId => get { getTotal(checkoutId) } ~ post { postTotal(checkoutId)}
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
    fulfillCheckoutRequest(checkoutId, completeAttachMember)
  }

  private def postItem(checkoutId: String) = {
    fulfillCheckoutRequest(checkoutId, completeAddItem)
  }

  private def getTotal(checkoutId: String) = {
    fulfillCheckoutRequest(checkoutId, completeGetTotal)
  }

  private def postTotal(checkoutId: String) = {
    fulfillCheckoutRequest(checkoutId, completeTotal)
  }

  private def fulfillCheckoutRequest(checkoutId: String, completeFn: Checkout => Route) = {
    findCheckout(checkoutId) map completeFn getOrElse completeCheckoutNotFound(checkoutId)
  }

  // This function is a mess. What smells does it exhibit?
  // Do some function extracts and renames.
  // Get rid of lies & clutter.
  // Take advantage of other constructs that already exist.
  // See how much you can improve it.
  private def createReceipt(retrievedCheckout: Checkout) = {
    var total = BigDecimal(0)
    var totalOfDiscountedItems = BigDecimal(0)
    var totalSaved = BigDecimal(0)

    val LineWidth = 45

    val lineItems = ListBuffer[String]()

    val discount = if (retrievedCheckout.member.isEmpty) BigDecimal(0) else retrievedCheckout.member.get.discount

    retrievedCheckout.items
      .foreach(item => {
        val price = item.price
        val isExempt = item.isExemptFromDiscount
        if (!isExempt && discount > 0) { // exempt form discount and pos. disc
          val discountAmount = discount * price
          val discountedPrice = price * (1.0 - discount)

          // add into total
          totalOfDiscountedItems += discountedPrice

          var text = item.description
          val amount = (price * 100 / 100).setScale(2).toString
          val amountWidth = amount.length
          var textWidth = LineWidth - amountWidth
          lineItems += pad(text, textWidth) + amount

          val discountPctFormatted = (discount * 100).round(new MathContext(0)).toInt
          val discountFormatted = "-" + discountAmount.setScale(2, RoundingMode.HALF_EVEN)
          textWidth = LineWidth - discountFormatted.length
          text = s"   ${discountPctFormatted}% mbr disc"
          lineItems += s"${pad(text, textWidth)}${discountFormatted}"

          total += discountedPrice

          totalSaved += discountAmount
        } else {
          // undiscounted
          val text = item.description
          val amount = price.setScale(2, RoundingMode.HALF_EVEN).toString // round
          val amountWidth = amount.length
          val textWidth = LineWidth - amountWidth
          lineItems += pad(text, textWidth) + amount

          total += item.price
        }
      })

    // Totals
    val amount = total.setScale(2, RoundingMode.HALF_EVEN).toString
    val amountWidth = amount.length
    val textWidth = LineWidth - amountWidth
    val totalLineItem = pad("TOTAL", textWidth) + amount
    var allLineItems = lineItems :+ totalLineItem

    if (totalSaved > 0) { // total saved
      val formattedTotal = totalSaved.setScale(2, RoundingMode.HALF_EVEN).toString
      val formattedTotalWidth = formattedTotal.length
      val textWidth = LineWidth - formattedTotalWidth
      allLineItems += pad("*** You saved:", textWidth) + formattedTotal
    }

    // misc. totals
    total = total.setScale(2, RoundingMode.HALF_EVEN)
    totalSaved = totalSaved.setScale(2, RoundingMode.HALF_EVEN)
    totalOfDiscountedItems = totalOfDiscountedItems.setScale(2, RoundingMode.HALF_EVEN)

    Receipt(total, totalSaved, totalOfDiscountedItems, allLineItems.toList)
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
    val checkout = Checkout(nextId().toString(), List(), Receipt(), None)
    checkouts += checkout
    complete(StatusCodes.Created, checkout.id)
  }

  private def findCheckout(checkoutId: String) = {
    checkouts.toList.find(checkout => checkout.id == checkoutId)
  }

  private def completeGetTotal(retrievedCheckout: Checkout) : Route = {
    val discount = retrievedCheckout.member map (member => member.discount) getOrElse BigDecimal(0)
    // could do group-by then reduce both...
    val total = retrievedCheckout.items.foldLeft(BigDecimal(0))(sumTotal(discount))
    complete(StatusCodes.Accepted, total.setScale(2).toString())
  }

  private def sumTotal(discount: BigDecimal) = {
    (total: BigDecimal, item: Item) =>
      val discountTo = if (item.isExemptFromDiscount) BigDecimal(1) else BigDecimal(1.0) - discount
      total + item.price * discountTo
  }

  private def completeTotal(checkout: Checkout) = {
    checkout.receipt = createReceipt(checkout)
    complete(StatusCodes.Accepted, checkout)
  }

  private def pad(s: String, length: Int) = {
    s + (" " * (length - s.length))
  }

  private def completeAddItem(retrievedCheckout: Checkout) = {
    entity(as[String]) { upc =>
      itemDatabase.retrieveItem(upc).map((item) => {
        retrievedCheckout.items = List.concat(retrievedCheckout.items, List(item))
        complete(StatusCodes.Accepted, item)
      })
      .getOrElse(completeUpcNotFound(upc))
    }
  }

  private def completeAttachMember(retrievedCheckout: Checkout) = {
    entity(as[String]) { phoneNumber =>
      memberDatabase.memberLookup(phoneNumber).map((member) => {
        retrievedCheckout.member = Some(member)
        complete(StatusCodes.Accepted)
      })
      .getOrElse(completeMemberNotFound(phoneNumber))
    }
  }

  private def completeCheckoutNotFound(checkoutId: String) = {
    complete(StatusCodes.NotFound, s"invalid checkout id: ${checkoutId}")
  }

  private def completeMemberNotFound(phoneNumber: String) = {
    complete(StatusCodes.NotFound, s"phone number not found: ${phoneNumber}")
  }

  private def completeUpcNotFound(upc: String) = {
    complete(StatusCodes.NotFound, s"invalid upc: ${upc}")
  }
}

object CheckoutRoutesImpl extends CheckoutRoutes {
  val itemDatabase = new Inventory()
  val memberDatabase = new MemberDatabase()
}

//  TODO val stuff: concurrent.Future[String] = Unmarshal(entity).to[String]
