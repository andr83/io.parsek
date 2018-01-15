package io.parsek.jdbc

import io.parsek.jdbc.instances._
import io.parsek.jdbc.syntax._

/**
  * @author Andrei Tupitcyn
  */
package object implicits extends ValueBinderInstances
  with ResultReaderInstances
  with ParameterBinderInstances
  with ParameterTypeMetaInstances
  with ColumnReaderInstances
  with ToSqlInstances
  with DataSourceSyntax

