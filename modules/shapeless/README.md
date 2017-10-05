##Shapeless module for parsek
 
Adds implicits conversions from case classes to PValue and back. 
Supports default values for case classes. 

Examples:

Converting case class to PValue

```$scala
scala> import io.parsek.shapeless.HListEncoder._;import io.parsek.instances.EncoderInstances._;import io.parsek.Encoder
import io.parsek.shapeless.HListEncoder._
import io.parsek.instances.EncoderInstances._
import io.parsek.Encoder

scala> case class MyClass(name: String, age: Int = 28) // our case class
defined class MyClass

scala> val t = implicitly[Encoder[MyClass]].apply(MyClass("Bulat"))
t: io.parsek.PValue = PMap(Map('age -> PInt(28), 'name -> PString(Bulat)))

```

Decoding case class from PValue

```
scala> import io.parsek._;import instances.EncoderInstances._;import instances.DecoderInstances._;import shapeless.HListDecoder._;import shapeless.HListEncoder._;import PValue._
import io.parsek._
import instances.EncoderInstances._
import instances.DecoderInstances._
import shapeless.HListDecoder._
import shapeless.HListEncoder._
import PValue._

// definition of case classes
scala> case class Address(address: String = "Parina", flat : Int= 34)
defined class Address

scala> case class Person(name: String, age: Int = 23, address: Address = Address(flat = 56))
defined class Person

// try to decode correct PValue
scala>  val t = implicitly[Decoder[Person]].apply(PValue.pmap('name -> PString("Bulat"), 'age -> PInt(28)))
t: io.parsek.Decoder.Result[Person] = Right(Person(Bulat,28,Address(Parina,56)))

// try to decode wrong PValue
scala>  val t = implicitly[Decoder[Person]].apply(PValue.pmap('name_ -> PString("Bulat"), 'age -> PInt(28)))
t: io.parsek.Decoder.Result[Person] = Left(java.lang.IllegalArgumentException: Field 'name does not exist in PMap)

```