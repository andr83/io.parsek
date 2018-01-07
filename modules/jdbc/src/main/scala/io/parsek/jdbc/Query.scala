package io.parsek.jdbc

/**
  * @author Andrei Tupitcyn
  */
case class Query(sql: String, params: Iterable[ParameterBinder] = Seq.empty[ParameterBinder]) {

  private val questionMarkRegex = "\\?(?=(([^']*?'[^']*?')*?[^']*?|([^']*?))$)".r

  /** Bind placeholder parameters to this query */
  def bind(params: PlaceholderValueBinder*): Query = buildQuery(sql, params)

  private def buildQuery(sql: String, binders: Seq[PlaceholderValueBinder]): Query = {
    val parts = questionMarkRegex.split(sql)

    val (_, builder) = parts.foldLeft(0 -> StringBuilder.newBuilder) {
      case ((index, sb), part) =>
        if (index < binders.length && !part.startsWith("'") && !part.startsWith("\"")) {
          val (fragment, _) = binders(index).toSql
          sb
            .append(part)
            .append(fragment)
        } else {
          sb.append(part)
        }
        index + 1 -> sb
    }
    Query(builder.toString, binders)
  }

  /** Optionally bind parameters */
  def bindOpt(params: Option[PlaceholderValueBinder]*): Query = buildQuery(sql, params.flatten)

  def as[A](implicit reader: ResultReader[A]): QueryIO[A] = QueryIO(this, reader)

  def as[A](columnIndex: Int)(implicit reader: ColumnReader[A]): QueryIO[A] = QueryIO(this, ResultReader.single(columnIndex))

  def as[A](columnName: String)(implicit reader: ColumnReader[A]): QueryIO[A] = QueryIO(this, ResultReader.single(columnName))

  def execute: ExecuteIO = ExecuteIO(this)

  def update: UpdateIO = UpdateIO(this)

  def batch(batchParams: Seq[Seq[ParameterBinder]])(implicit qe: QueryExecutor): Array[Int] = qe.executeBatch(this, batchParams)
}