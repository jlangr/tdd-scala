package com.langrsoft.pos

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

case class Member(id: String, phoneNumber: String, name: String, discount: BigDecimal)

case class Item(id: String, upc: String, description: String, price: BigDecimal)

case class Checkout(id: String, var memberId: String, var member: Member, var items: List[Item])

object CheckoutJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val aMember = jsonFormat4(Member)

  implicit val anItem = jsonFormat4(Item)

  implicit object CheckoutJsonFormat extends RootJsonFormat[Checkout] {
    def write(checkout: Checkout) = {
      var fields: Map[String,JsValue] = null
      // TODO fix this
      if (checkout.member == null) {
        fields = Map(
          "id" -> JsString(checkout.id),
          "memberId" -> JsString(checkout.memberId),
          "items" -> JsArray(checkout.items.map(_.toJson).toVector)
        )
      }
      else {
        fields = Map(
          "id" -> JsString(checkout.id),
          "memberId" -> JsString(checkout.memberId),
          "member" -> checkout.member.toJson,
          "items" -> JsArray(checkout.items.map(_.toJson).toVector)
        )
      }
      JsObject(fields)
    }

    def read(value: JsValue): Checkout = {
      value.asJsObject.getFields("id", "memberId", "member", "items") match {
        case Seq(JsString(id), JsString(description), member, JsArray(items)) =>
          new Checkout(id, description, member.convertTo[Member], items.map(_.convertTo[Item]).to[List])
        case Seq(JsString(id), JsString(description), JsArray(items)) =>
          new Checkout(id, description, null, items.map(_.convertTo[Item]).to[List])
      }
    }
  }
}
