package io.parsek.jdbc

import io.parsek.jdbc.instances._

/**
  * @author Andrei Tupitcyn
  */
package object implicits extends ValueBinderInstances
  with ResultReaderInstances
  with ColumnReaderInstances
  with ToSqlInstances
  with io.parsek.jdbc.syntax.DataSourceSyntax

