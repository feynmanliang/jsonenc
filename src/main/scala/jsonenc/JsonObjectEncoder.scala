package jsonenc

import shapeless.{::, :+:, CNil, Coproduct, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness}
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

  implicit val cnilEncoder: JsonObjectEncoder[CNil] =
    createObjectEncoder(_ => throw new Exception("Impossible!"))

  implicit def coproductEncoder[K <: Symbol, H, T <: Coproduct](
    implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[JsonEncoder[H]], // `Lazy` important because JsonObject is recursive, prevent implicit divergence
    tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :+: T] = {
      val fieldName = witness.value.name
      createObjectEncoder {
        case Inl(h) =>
          JsonObject(List(fieldName -> hEncoder.value.encode(h)))
        case Inr(t) =>
          tEncoder.encode(t)
      }
  }

  implicit def genericObjectEncoder[A, H](
    implicit
    generic: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[JsonObjectEncoder[H]]
  ): JsonEncoder[A] =
    createObjectEncoder { value =>
      hEncoder.value.encode(generic.to(value))
    }
}
