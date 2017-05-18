package io.parsek.jackson

import java.time.Instant

import org.scalatest.{FlatSpec, Matchers}
import io.parsek._

/**
  * @author andr83
  */
class JsonSpec extends FlatSpec with Matchers {
  "Json SerDe" should "parse string to PValue and serialize back" in {
    val str =
      """{"data":{"currency":"Kč"},"sessionId":"af652b63d57c6cb508fd9176ffkf65e48c78ef38","created":1467158651342}"""

    val serde = JsonSerDe()
    val pv = serde.read(str)
    val json = serde.write(pv)
    assert(str == json)
  }

  it should "format dates according format" in {
    val now = Instant.now()
    val value = pmap('time -> PValue.fromInstant(now))

    val formatter = InstantFormatter("yyyy-mm-dd")
    val serde = JsonSerDe(formatter)
    val json = serde.write(value)

    json shouldBe s"""{"time":"${formatter.format(now).right.get}"}"""
  }
}
