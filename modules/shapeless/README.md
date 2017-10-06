##Shapeless module for parsek
 
Adds implicits conversions from products (case classes) to PValue and back. 
Supports default values for case classes. 

###Examples:

Converting case class to PValue

```$scala
scala> import io.parsek.implicits._;import io.parsek.shapeless.implicits._

scala> case class MyClass(name: String, age: Int = 28) // our case class
defined class MyClass

scala> val t = MyClass("Bulat").toPValue
t: io.parsek.PValue = PMap(Map('age -> PInt(28), 'name -> PString(Bulat)))

```

Decoding case class from PValue

```
scala> import io.parsek.implicits._;import io.parsek.shapeless.implicits._;import io.parsek._

// definition of case classes
scala> case class Address(address: String = "Parina", flat : Int= 34)
defined class Address

scala> case class Person(name: String, age: Int = 23, address: Address = Address(flat = 56))
defined class Person

// try to decode correct PValue
scala>  val t = PValue.pmap('name -> PValue.PString("Bulat"), 'age -> PValue.PInt(28)).as[Person]
t: io.parsek.Decoder.Result[Person] = Right(Person(Bulat,28,Address(Parina,56)))

// try to decode wrong PValue
scala>  val t = PValue.pmap('name_ -> PValue.PString("Bulat"), 'age -> PValue.PInt(28)).as[Person]
t: io.parsek.Decoder.Result[Person] = Left(java.lang.IllegalArgumentException: Field 'name does not exist in PMap)

```