package io.parsek.jdbc

import java.sql.ResultSet

import io.parsek.{Decoder, PValue}

/**
  * @author Andrei Tupitcyn
  */
case class Query(sql: String, params: Iterable[ParameterBinder] = Seq.empty[ParameterBinder]) {

  private val questionMarkRegex = "[^\\?\"']+|\"([^\"]*)\"|'([^']*)'".r
  private def buildQuery(sql: String, binders: Seq[PlaceholderValueBinder]): Query = {
    val parts = for(
      m <- questionMarkRegex.findAllMatchIn(sql)
    ) yield {
      m.group(0)
    }

    val (_, builder) = parts.foldLeft(0 -> StringBuilder.newBuilder) {
      case ((index, sb), part) =>
        if (index < binders.length) {
          val (fragment, _) = binders(index).toSql
          sb
            .append(part)
            .append(fragment)
        } else {
          sb.append(part)
        }
        index + 1 -> sb
    }
    Query(builder.toString,binders)
  }

  def bind(params: PlaceholderValueBinder*): Query = buildQuery(sql, params)
  def bindOpt(params: Option[PlaceholderValueBinder]*): Query = buildQuery(sql, params.flatten)

  def asList[A: Decoder](implicit qe: QueryExecutor): List[A] = {
    val d = implicitly[Decoder[A]]
    list(qe).map(r => d(r) match {
      case Right(a) => a
      case Left(err) => throw err
    })
  }

  def list(implicit qe: QueryExecutor): List[PValue] = {
    val b = List.newBuilder[PValue]
    qe.executeQuery(this)(rs => {
      val e: ResultSet => PValue = qe.encoder.encode(rs.getMetaData).apply
      while (rs.next()) {
        b += e(rs)
      }
      b.result()
    })
  }

  def asList[A: Decoder](i: Int)(implicit qe: QueryExecutor): List[A] = {
    val d = implicitly[Decoder[A]]
    list(i)(qe).map(r => d(r) match {
      case Right(a) => a
      case Left(err) => throw err
    })
  }

  def list(i: Int)(implicit qe: QueryExecutor): List[PValue] = {
    val b = List.newBuilder[PValue]
    qe.executeQuery(this)(rs => {
      val e: ResultSet => PValue = qe.encoder.encode(i, rs.getMetaData).apply
      while (rs.next()) {
        b += e(rs)
      }
      b.result()
    })
  }

  def map[A: Decoder, B](f: A => B)(implicit qe: QueryExecutor): List[B] = {
    val b = List.newBuilder[B]
    qe.executeQuery(this)(rs => {
      val e: ResultSet => PValue = qe.encoder.encode(rs.getMetaData).apply
      val d = implicitly[Decoder[A]]
      while (rs.next()) {
        b += f(d.unsafe(e(rs)))
      }
      b.result()
    })
  }

  def foreach[A: Decoder](f: A => Unit)(implicit qe: QueryExecutor): Unit = {
    val d = implicitly[Decoder[A]]
    qe.executeQuery[Unit](this)(rs=> {
      val e = qe.encoder.encode(rs.getMetaData)
      while (rs.next()) {
        f(d.unsafe(e(rs)))
      }
    })
  }

  def as[A: Decoder](i: Int)(implicit qe: QueryExecutor): A =
    implicitly[Decoder[A]].apply(col(i)) match {
      case Right(a) => a
      case Left(err) => throw err
    }

  def col(i: Int)(implicit qe: QueryExecutor): PValue = {
    qe.executeQuery(this)(rs => {
      if (rs.next()) {
        qe.encoder.encode(i, rs.getMetaData)(rs)
      } else {
        PValue.Null
      }
    })
  }

  def as[A: Decoder](implicit qe: QueryExecutor): A =
    implicitly[Decoder[A]].apply(row) match {
      case Right(a) => a
      case Left(err) => throw err
    }

  def row(implicit qe: QueryExecutor): PValue = {
    qe.executeQuery(this)(rs => {
      if (rs.next()) {
        qe.encoder.encode(rs.getMetaData)(rs)
      } else {
        PValue.Null
      }
    })
  }

  def execute(implicit qe: QueryExecutor): Boolean = qe.execute(this)

  def update(implicit qe: QueryExecutor): Int = qe.executeUpdate(this)

  def batch(batchParams: Seq[Seq[ParameterBinder]])(implicit qe: QueryExecutor): Array[Int] = qe.executeBatch(this, batchParams)
}