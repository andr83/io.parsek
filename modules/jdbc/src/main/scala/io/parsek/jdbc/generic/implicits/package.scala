package io.parsek.jdbc.generic

/**
  * @author andr83 
  *         created on 10.08.17
  */
package object implicits
  extends io.parsek.jdbc.generic.instances.ValueBinderInstances
  with instances.ToSqlInstances
  with syntax.DataSourceSyntax

