package jsonenc

import shapeless.{HList, ::, HNil, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.FieldType

trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  def encode(value: A): JsonObject
}

/**
 * Encoder for (labelled) generic `HList`s.
 */
object JsonObjectEncoder {
  def createObjectEncoder[A](f: A => JsonObject): JsonObjectEncoder[A] =
    new JsonObjectEncoder[A] {
      def encode(value: A): JsonObject = f(value)
    }

  implicit val hnilEncoder: JsonObjectEncoder[HNil] =
    createObjectEncoder(_ => JsonObject(Nil))

  implicit def hlistEncoder[K <: Symbol, H, T <: HList](
    implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[JsonEncoder[H]], // `Lazy` important because JsonObject is recursive, prevent implicit divergence
    tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :: T] = {
      val fieldName = witness.value.name
      createObjectEncoder { hlist =>
        val head = hEncoder.value.encode(hlist.head)
        val tail = tEncoder.encode(hlist.tail)
        JsonObject(fieldName -> head :: tail.fields)
      }
  }

  implicit def genericObjectEncoder[A, H <: HList](
    implicit
    generic: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[JsonObjectEncoder[H]]
  ): JsonEncoder[A] =
    createObjectEncoder { value =>
      hEncoder.value.encode(generic.to(value))
    }
}
