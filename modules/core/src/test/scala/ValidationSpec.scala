import io.parsek._
import io.parsek.implicits._
import io.parsek.types._
import org.scalatest.{FlatSpec, Matchers}

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
      :: PStructField('fArray, PArrayType(Some(PIntType)))
      :: Nil
  )

  "Validation" should "pass without warnings" in {
    val res = validate(testValue, scheme)
    res shouldBe PResult.valid(testValue)

    val res2 = validate(testValue, scheme.add('fInt, PStringType))
    res2 shouldBe PResult.valid(testValue.update('fInt, PValue.fromString("10")))
  }

  it should "pass with warnings" in {
    val res = validate(testValue, scheme.add('fInt, PBooleanType))
    res shouldBe PResult.valid(testValue.update('fInt, PValue.Null))

    val res2 = validate(testValue, scheme.add('fOpt, PStringType))
    res2 shouldBe PResult.valid(testValue.update('fOpt, PValue.Null))
  }
}
