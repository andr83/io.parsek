package io.parsek.jdbc

import java.sql.ResultSet

import io.parsek.{Decoder, PValue}

/**
  * @author Andrei Tupitcyn
  */
case class Query(sql: String, params: Iterable[ParameterBinder] = Seq.empty[ParameterBinder]) {

  def bind(binds: ParameterBinder*): Query = this.copy(sql, binds)

  def withParams(params: Iterable[Any]): Query = this.copy(sql, params.map(ParameterBinder.apply))

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