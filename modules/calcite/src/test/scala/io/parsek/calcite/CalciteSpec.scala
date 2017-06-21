package io.parsek.calcite

import java.sql.DriverManager
import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneId}
import java.util.Properties

import io.parsek.PValue.PMap
import io.parsek._
import io.parsek.calcite.adapter.ParsekTable
import io.parsek.implicits._
import io.parsek.jdbc._
import io.parsek.jdbc.generic.JdbcQueryExecutor
import io.parsek.types._
import org.apache.calcite.jdbc.CalciteConnection
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Andrei Tupitcyn
  */
class CalciteSpec extends FlatSpec with Matchers {
  val scheme = PStructType(
    PStructField('bool_field, PBooleanType)
      :: PStructField('int_field, PIntType)
      :: PStructField('long_field, PLongType)
      :: PStructField('time_field, PInstantType)
      :: PStructField('double_field, PDoubleType)
      :: PStructField('string_field, PStringType)
      :: Nil
  )

  val instant = LocalDateTime
    .parse("2000-01-01 13:59:12", DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
    .atZone(ZoneId.of("UTC"))
    .toInstant

  val r1: PMap = pmap(
    'int_field -> PValue(10),
    'long_field -> PValue(9876543210L),
    'double_field -> PValue(45.7),
    'bool_field -> PValue.True,
    'time_field -> PValue(instant),
    'string_field -> PValue("hello world!")
  )

  val r2: PMap = pmap(
    'int_field -> PValue(11),
    'long_field -> PValue(-4790L),
    'double_field -> PValue(1.12435),
    'bool_field -> PValue.False,
    'time_field -> PValue(instant),
    'string_field -> PValue("who am I?")
  )

  def withQueryExecutor(test: ParsekTable => QueryExecutor => Unit): Unit = {
    Class.forName("org.apache.calcite.jdbc.Driver")
    val info = new Properties()
    info.setProperty("lex", "JAVA")
    info.setProperty("timezone", "UTC")

    val connection = DriverManager.getConnection("jdbc:calcite:", info)
    val calciteConnection: CalciteConnection = connection.asInstanceOf[CalciteConnection]
    val schema = calciteConnection.getRootSchema
    val table = ParsekTable(scheme, "int_field" :: Nil)
    schema.add("test", table)

    val qe = JdbcQueryExecutor(calciteConnection, LowerCaseConverter)
    try {
      test(table)(qe)
    } finally {
      calciteConnection.close()
    }
  }

  "Calcite adapter" should "query over PValue data" in {
    withQueryExecutor { table => implicit qe =>
      table.add(r1)

      val res = sql"select * from test".row
      res shouldBe r1

      table.add(r2)
      val res2 = sql"select * from test where int_field = 11".row
      res2 shouldBe r2

      val res3 = sql"select count(1) from test".as[Int](1)
      res3 shouldBe 2

      (1 to 100000) foreach (i => {
        table.add(r1.update('int_field, PValue.fromInt(i)))
      })

      val res4 = sql"select count(1) from test".as[Int](1)
      res4 shouldBe 100000

      val strVal = r1.value.get('string_field).get.as[String].right.get
      val start = System.nanoTime()
      val res5 = sql"select * from test where int_field > 90000 and int_field < 90150 and string_field = $strVal".list
      println(s"Time: ${(System.nanoTime() - start) / 1000000000.0}")
      res5 should have size 149
    }
  }

  it should "compact rows by pk" in {
    withQueryExecutor { table => implicit qe =>
      table.add(r1)

      val res = sql"select count(1) from test".as[Int](1)
      res shouldBe 1

      val r3 = r1.update('long_field, PValue.fromLong(123L))
      table.add(r3)
      val res2 = sql"select count(1) from test".as[Int](1)
      res2 shouldBe 1

      val res3 = sql"select * from test where int_field=10".row
      res3 shouldBe r3

      table.remove(r1)
      val res4 = sql"select count(1) from test".as[Int](1)
      res4 shouldBe 0
    }
  }

}
