package jsonenc

import shapeless.LabelledGeneric
import shapeless._
import shapeless.labelled._

import org.scalatest._

import JsonEncoder._
import JsonObjectEncoder._

class JsonEncoderSpec extends FlatSpec with Matchers {
  "A JsonEncoder" should "encode products" in {
    case class IceCream(name: String, numCherries: Int, inCone: Boolean)

    val iceCream = IceCream("Sundae", 1, false)

    JsonEncoder[IceCream].encode(iceCream) shouldBe JsonObject(List(
      ("name", JsonString("Sundae")),
      ("numCherries", JsonNumber(1)),
      ("inCone", JsonBoolean(false)),
    ))
  }

  "A JsonEncoder" should "encode coproducts" in {
    sealed trait Shape
    final case class Rectangle(width: Double, height: Double) extends Shape
    final case class Circle(radius: Double) extends Shape

    JsonEncoder[Shape].encode(Circle(1.0).asInstanceOf[Shape]) shouldBe JsonObject(List(
      "Circle" -> JsonObject(List(
        ("radius", JsonNumber(1.0))
      ))
    ))

    JsonEncoder[Shape].encode(Rectangle(1.5, 2.5).asInstanceOf[Shape]) shouldBe JsonObject(List(
      "Rectangle" -> JsonObject(List(
        ("width", JsonNumber(1.5)),
        ("height", JsonNumber(2.5))
      ))
    ))
  }
}
