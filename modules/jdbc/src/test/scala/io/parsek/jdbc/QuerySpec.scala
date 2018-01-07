package io.parsek.jdbc

import java.sql.DriverManager
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, ZoneId}

import io.parsek.PValue.PMap
import io.parsek._
import io.parsek.implicits._
import io.parsek.jdbc.implicits._
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Andrei Tupitcyn
  */
class QuerySpec extends FlatSpec with Matchers {

  val instant = LocalDateTime
    .parse("2000-01-01 13:59:12", DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
    .atZone(ZoneId.systemDefault())
    .toInstant

  val today = LocalDate.now
  val yesterday = LocalDate.now().minusDays(1)

  val r1: PMap = pmap(
    'int_field -> PValue(10),
    'long_field -> PValue(9876543210L),
    'bool_field -> PValue.True,
    'time_field -> PValue(instant),
    'date_field -> PValue(today),
    'string_field -> PValue("hello world!"),
    'array_field -> PValue(Vector(
      PValue.fromInt(1),
      PValue.fromInt(2),
      PValue.fromInt(3))
    )
  )

  val r2: PMap = pmap(
    'int_field -> PValue(11),
    'long_field -> PValue(-4790L),
    'bool_field -> PValue.False,
    'time_field -> PValue(instant),
    'date_field -> PValue(yesterday),
    'string_field -> PValue("who am I?"),
    'array_field -> PValue(Vector(
      PValue.fromInt(5),
      PValue.fromInt(6),
      PValue.fromInt(7))
    )
  )

  def fromIO[A](test: => JdbcIO[A]): Unit = {
    Class.forName("org.h2.Driver")
    val conn = DriverManager.getConnection("jdbc:h2:mem:test")
    val qe = JdbcQueryExecutor(conn, LowerCaseConverter)
    try {
      val stmt = conn.createStatement()
      stmt.executeUpdate(
        """
          |CREATE TABLE test (
          | int_field INTEGER NOT NULL,
          | long_field BIGINT,
          | bool_field BOOLEAN,
          | time_field TIMESTAMP,
          | date_field DATE,
          | string_field VARCHAR(250),
          | array_field ARRAY
          |)
        """.stripMargin)
      stmt.close()

      (for {
        _ <- JdbcIO.insert("TEST", r1)
        _ <- JdbcIO.insert("TEST", r2)
        res <- test
      } yield res).unsafeRun(qe)
    } finally {
      conn.close()
    }
  }

  "Query" should "be created from string interpolation" in {
    val min = 10
    val q = sql"select * from test where int_field > $min"
    q.sql shouldBe "select * from test where int_field > ?"
    q.params.size shouldBe 1
  }

  it should "be able run queries in monadic flow" in fromIO {
    for {
      res1 <- sql"select * from test where int_field = 10".as[Int]
      res2 <- sql"select * from test where int_field in (?)".bind(Seq(10, 11)).as[List[PValue]]
      res3 <- sql"select * from test where int_field in (${Seq(10, 11)})".as[List[PValue]]
      res4 <- sql"select * from test".as[List[PValue]]
      res5 <- sql"select * from test where int_field = ?".bindOpt(Some(10), None).as[PValue]
    } yield {
      res1 shouldBe 10
      res2 shouldBe r1 :: r2 :: Nil
      res3 shouldBe r1 :: r2 :: Nil
      res4 shouldBe r1 :: r2 :: Nil
      res5 shouldBe r1
    }
  }

  "QueryExecutor" should "insert PValue to table" in fromIO {
    for {
      res1 <- sql"delete from test".update
      res2 <- sql"select count(*) from test".as[Int](1)
      _ <- JdbcIO.insert("TEST", r1)
      res3 <- sql"select count(*) from test".as[Int](1)
      _ <- JdbcIO.insert("TEST", r2)
      res4 <- sql"select * from test".as[List[PValue]]
      _ <- sql"insert into test(int_field, array_field) values (${r1.int_field}, ${r1.array_field})".update
      res5 <- sql"select count(1) from test".as[Int](1)
    } yield {
      res1 shouldBe 2
      res2 shouldBe 0
      res3 shouldBe 1
      res4 shouldBe r1 :: r2 :: Nil
      res5 shouldBe 3
    }
  }

  it should "insert values in batch mode" in fromIO {
    for {
      res1 <- sql"delete from test".update
      _ <- JdbcIO.batchInsert("TEST", Seq(r1, r2))
      res2 <- sql"select * from test".as[List[PValue]]
    } yield {
      res1 shouldBe 2
      res2 shouldBe r1 :: r2 :: Nil
    }
  }

  it should "work proper with quotes" in {
    val q = Query(
      "select * from test where id = ? and test = \"dsf?dfs\"")
      .bind(1, 2)
    q.sql shouldBe "select * from test where id = ? and test = \"dsf?dfs\""
  }
}
