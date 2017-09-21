import io.parsek.Decoder


trait HListDecoders {
  import shapeless.HList
  implicit def hlistDecoder[A: Decoder, +T <: HList : Decoder]:Decoder[A :: T]

  implicit def hnillDecoder: Decoder[HNil]
}
