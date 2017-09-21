import io.parsek.{Decoder, PValue}


trait HListDecoders {
  import shapeless.HList
  import shapeless.::
  implicit def hlistDecoder[A: Decoder, +T <: HList : Decoder]:Decoder[A :: T] = new Decoder[A:: T]{
    override def apply(v: PValue) = ???
  }

  implicit def hnillDecoder: Decoder[HNil]
}
