package io.parsek.calcite

import java.sql.DriverManager
import java.util.Properties

import io.parsek.PValue.{PInt, PMap}
import io.parsek._
import io.parsek.calcite.adapter.ParsekTable
import io.parsek.jdbc._
import io.parsek.jdbc.generic.JdbcQueryExecutor
import io.parsek.types._
import org.apache.calcite.jdbc.CalciteConnection
import org.scalameter._

import scala.util.Random

/**
  * @author andr83 
  *         created on 21.06.17
  */
object CalciteBenchmark {
  def newValue(id: Int, transactionId: Long, name: String): PMap = pmap(
    'id -> PValue.fromInt(id),
    'transaction_id -> PValue.fromLong(transactionId),
    'name -> PValue.fromString(name)
  )

  val ranges: Gen[Int] = Gen.range("id_range")(0, 1000000, 100000)

  def genValues(n: Int): Seq[PMap] = {
    val randTransId = new Random(3)
    val randName = new Random(5)

    (0 until n)
      .map(i=> newValue(i, randTransId.nextLong(), Random.alphanumeric.take(randName.nextInt(300)).mkString))
  }

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer new Warmer.Default

  val scheme = PStructType(
    PStructField('id, PIntType)
      :: PStructField('transaction_id, PLongType)
      :: PStructField('name, PStringType)
      :: Nil
  )

  def withQueryExecutor(test: ParsekTable => QueryExecutor => Unit): Unit = {
    Class.forName("org.apache.calcite.jdbc.Driver")
    val info = new Properties()
    info.setProperty("lex", "JAVA")
    info.setProperty("timezone", "UTC")

    val connection = DriverManager.getConnection("jdbc:calcite:", info)
    val calciteConnection: CalciteConnection = connection.asInstanceOf[CalciteConnection]
    val schema = calciteConnection.getRootSchema
    val table = ParsekTable(scheme, 'id :: Nil)
    schema.add("test", table)

    val qe = JdbcQueryExecutor(calciteConnection, LowerCaseConverter)
    try {
      test(table)(qe)
    } finally {
      calciteConnection.close()
    }
  }

  def main(args: Array[String]): Unit = {
    val data = genValues(100000)
    val scalaNativeTime: Quantity[Double] = standardConfig measure {
      data.filter(_.value.get('id).exists({case PInt(id) => id == 999}))
    }

    withQueryExecutor{table=> implicit qe=>
      data.foreach(table.add)

      val calciteTime = standardConfig measure {
        sql"SELECT * from test where id = 999".list
      }
      println(s"Scala collection time: $scalaNativeTime")
      println(s"Calcite time: $calciteTime")
      println(s"Calcite slower in : ${calciteTime.value / scalaNativeTime.value}")
    }
  }
}
