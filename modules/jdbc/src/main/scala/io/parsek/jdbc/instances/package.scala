package io.parsek.jdbc

/**
  * @author Andrei Tupitcyn
  */
package object instances {
  object columnReader extends ColumnReaderInstances
  object resultReader extends ResultReaderInstances
  object parameterBinder extends ParameterBinderInstances
  object parameterTypeMeta extends ParameterTypeMetaInstances
  object toSql extends ToSqlInstances
  object valueBinder extends ValueBinderInstances
}
