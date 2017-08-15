package io.parsek.jdbc

import java.sql.ResultSet

import io.parsek.{Decoder, PValue}

/**
  * @author Andrei Tupitcyn
  */
case class Query(sql: String, params: Iterable[ParameterBinder] = Seq.empty[ParameterBinder]) {

  def withParams(params: Iterable[Any])(implicit binder: ValueBinder[Any]): Query = this.copy(sql, params.map(binder.apply))

  def on[A1 : ValueBinder](a1: A1): Query = this.copy(sql, params.toSeq :+ implicitly[ValueBinder[A1]].apply(a1))
  def on[A1 : ValueBinder, A2 : ValueBinder](a1: A1, a2: A2): Query = this.copy(sql, params.toSeq ++ Seq(implicitly[ValueBinder[A1]].apply(a1), implicitly[ValueBinder[A2]].apply(a2)))
  def on[A1 : ValueBinder, A2 : ValueBinder, A3 : ValueBinder](a1: A1, a2: A2, a3: A3): Query = this.copy(sql, params.toSeq ++ Seq(implicitly[ValueBinder[A1]].apply(a1), implicitly[ValueBinder[A2]].apply(a2), implicitly[ValueBinder[A3]].apply(a3)))
  def on[A1 : ValueBinder, A2 : ValueBinder, A3 : ValueBinder, A4 : ValueBinder](a1: A1, a2: A2, a3: A3, a4: A4): Query = this.copy(sql, params.toSeq ++ Seq(implicitly[ValueBinder[A1]].apply(a1), implicitly[ValueBinder[A2]].apply(a2), implicitly[ValueBinder[A3]].apply(a3), implicitly[ValueBinder[A4]].apply(a4)))
  def on[A1 : ValueBinder, A2 : ValueBinder, A3 : ValueBinder, A4 : ValueBinder, A5 : ValueBinder](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5): Query = this.copy(sql, params.toSeq ++ Seq(implicitly[ValueBinder[A1]].apply(a1), implicitly[ValueBinder[A2]].apply(a2), implicitly[ValueBinder[A3]].apply(a3), implicitly[ValueBinder[A4]].apply(a4),implicitly[ValueBinder[A5]].apply(a5)))
  def on[A1 : ValueBinder, A2 : ValueBinder, A3 : ValueBinder, A4 : ValueBinder, A5 : ValueBinder, A6 : ValueBinder](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6): Query = this.copy(sql, params.toSeq ++ Seq(implicitly[ValueBinder[A1]].apply(a1), implicitly[ValueBinder[A2]].apply(a2), implicitly[ValueBinder[A3]].apply(a3), implicitly[ValueBinder[A4]].apply(a4),implicitly[ValueBinder[A5]].apply(a5), implicitly[ValueBinder[A6]].apply(a6)))
  def on[A1 : ValueBinder, A2 : ValueBinder, A3 : ValueBinder, A4 : ValueBinder, A5 : ValueBinder, A6 : ValueBinder, A7 : ValueBinder](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7): Query = this.copy(sql, params.toSeq ++ Seq(implicitly[ValueBinder[A1]].apply(a1), implicitly[ValueBinder[A2]].apply(a2), implicitly[ValueBinder[A3]].apply(a3), implicitly[ValueBinder[A4]].apply(a4),implicitly[ValueBinder[A5]].apply(a5), implicitly[ValueBinder[A6]].apply(a6), implicitly[ValueBinder[A7]].apply(a7)))
  def on[A1 : ValueBinder, A2 : ValueBinder, A3 : ValueBinder, A4 : ValueBinder, A5 : ValueBinder, A6 : ValueBinder, A7 : ValueBinder, A8 : ValueBinder](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8): Query = this.copy(sql, params.toSeq ++ Seq(implicitly[ValueBinder[A1]].apply(a1), implicitly[ValueBinder[A2]].apply(a2), implicitly[ValueBinder[A3]].apply(a3), implicitly[ValueBinder[A4]].apply(a4),implicitly[ValueBinder[A5]].apply(a5), implicitly[ValueBinder[A6]].apply(a6), implicitly[ValueBinder[A7]].apply(a7), implicitly[ValueBinder[A8]].apply(a8)))
  def on[A1 : ValueBinder, A2 : ValueBinder, A3 : ValueBinder, A4 : ValueBinder, A5 : ValueBinder, A6 : ValueBinder, A7 : ValueBinder, A8 : ValueBinder, A9 : ValueBinder](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9): Query = this.copy(sql, params.toSeq ++ Seq(implicitly[ValueBinder[A1]].apply(a1), implicitly[ValueBinder[A2]].apply(a2), implicitly[ValueBinder[A3]].apply(a3), implicitly[ValueBinder[A4]].apply(a4),implicitly[ValueBinder[A5]].apply(a5), implicitly[ValueBinder[A6]].apply(a6), implicitly[ValueBinder[A7]].apply(a7), implicitly[ValueBinder[A8]].apply(a8), implicitly[ValueBinder[A9]].apply(a9)))

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