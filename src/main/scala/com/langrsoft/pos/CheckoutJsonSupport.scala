package com.langrsoft.pos

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

case class Member(id: String, phoneNumber: String, name: String, discount: BigDecimal)

case class Item(id: String, upc: String, description: String, price: BigDecimal, isExemptFromDiscount: Boolean)

case class Receipt(var total: BigDecimal = BigDecimal(0.0),
                   var totalSaved: BigDecimal = BigDecimal(0.0),
                   var totalOfDiscountedItems: BigDecimal = BigDecimal(0.0),
                   var lineItems: List[String] = List())

case class Checkout(id: String, var items: List[Item], var receipt: Receipt, var member: Option[Member])

object CheckoutJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val aTotals = jsonFormat4(Receipt)

  implicit val aMember = jsonFormat4(Member)

  implicit val anItem = jsonFormat5(Item)

  implicit object CheckoutJsonFormat extends RootJsonFormat[Checkout] {
    def write(checkout: Checkout) = {
      var fields: Map[String,JsValue] = Map(
        "id" -> JsString(checkout.id),
        "items" -> JsArray(checkout.items.map(_.toJson).toVector),
        "receipt" -> checkout.receipt.toJson)
      if (!checkout.member.isEmpty) fields += ("member" -> checkout.member.toJson)
      JsObject(fields)
    }

    def read(value: JsValue): Checkout = {
      value.asJsObject.getFields("id", "items", "receipt", "member") match {
        case Seq(JsString(id), JsArray(items), totals, member) =>
          new Checkout(id, items.map(_.convertTo[Item]).to[List], totals.convertTo[Receipt], member.convertTo[Option[Member]])
        case Seq(JsString(id), JsArray(items), totals) =>
          new Checkout(id, items.map(_.convertTo[Item]).to[List], totals.convertTo[Receipt], None)
      }
    }
  }
}
