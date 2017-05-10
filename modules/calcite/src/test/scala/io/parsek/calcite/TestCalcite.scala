package io.parsek.calcite

import java.sql.DriverManager
import java.util
import java.util.{Properties, List => JList}

import io.parsek.PValue.PMap
import io.parsek._
import io.parsek.calcite.adapter.ParsekTable
import org.apache.calcite.jdbc.CalciteConnection
import org.apache.calcite.schema.SchemaPlus
import org.apache.calcite.tools.Frameworks

/**
  * @author Andrei Tupitcyn
  */
object TestCalcite {
  def main(args: Array[String]): Unit = {
    val defaultSchema: SchemaPlus = Frameworks.createRootSchema(true)

    val data = new util.ArrayList[PMap]()
    data.add(pmap("name" -> PValue("My name")))

    defaultSchema.add("test", new ParsekTable(data))

    Class.forName("org.apache.calcite.jdbc.Driver")
    val info = new Properties()
    info.setProperty("lex", "JAVA")

    val connection = DriverManager.getConnection("jdbc:calcite:", info)
    val calciteConnection: CalciteConnection = connection.asInstanceOf[CalciteConnection]
    val schema = calciteConnection.getRootSchema
    schema.add("test", new ParsekTable(data))

    val stmt = calciteConnection.createStatement()

    val rs = stmt.executeQuery("select * from test")
    rs.next()
    println(rs.getString("name"))
    rs.close()

    data.add(pmap("name" -> PValue("Your name")))
    val rs2 = stmt.executeQuery("select count(1) from test")
    rs2.next()
    println(rs2.getInt(1))
    rs2.close()
  }
}
