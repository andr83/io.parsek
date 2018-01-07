package io.parsek.jdbc

import io.parsek.PValue
import io.parsek.PValue.PMap

/**
  * @author Andrei Tupitcyn
  */
case class NamedQueryBuilder(sql: String, binders: Iterable[(String, PValueBinder)], nameConverter: NameConverter) {
  def query(r: PMap): Query = Query(sql, binders.map {
    case (k, b) => b(r.value.getOrElse(Symbol(nameConverter(k)), PValue.Null))
  })

}
