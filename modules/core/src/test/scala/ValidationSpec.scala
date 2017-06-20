import cats.data.Validated.valid
import io.parsek._
import io.parsek.implicits._
import io.parsek.types._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

/**
  * @author andr83
  */
class ValidationSpec extends FlatSpec with Matchers {
  val testValue = pmap(
    'fBool -> PValue(true),
    'fInt -> PValue(10),
    'fLong -> PValue(100L),
    'fDouble -> PValue(12.3),
    'fString -> PValue("hello"),
    'fArray -> PValue(List(1, 2, 3))
  )

  val scheme = PStructType(
    PStructField('fBool, PBooleanType)
      :: PStructField('fInt, PIntType)
      :: PStructField('fLong, PLongType)
      :: PStructField('fDouble, PDoubleType)
      :: PStructField('fString, PStringType)
      :: PStructField('fArray, PArrayType)
      :: Nil
  )

  "Validation" should "pass without warnings" in {
    val warnings = mutable.ArrayBuffer.empty[Throwable]

    val res = validate(testValue, scheme, warnings)
    res shouldBe valid(testValue)
    warnings shouldBe empty

    val res2 = validate(testValue, scheme.add('fInt, PStringType), warnings)
    res2 shouldBe valid(testValue.update('fInt, PValue.fromString("10")))
    warnings shouldBe empty
  }

  it should "pass with warnings" in {
    val warnings = mutable.ArrayBuffer.empty[Throwable]

    val res = validate(testValue, scheme.add('fInt, PBooleanType), warnings)
    res shouldBe valid(testValue.update('fInt, PValue.Null))
    warnings should have size 1

    warnings.clear()
    val res2 = validate(testValue, scheme.add('fOpt, PStringType), warnings)
    res2 shouldBe valid(testValue.update('fOpt, PValue.Null))
    warnings should have size 1
  }
}
