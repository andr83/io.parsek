# io.parsek
[![Build Status](https://travis-ci.org/andr83/io.parsek.svg?branch=master)](https://travis-ci.org/andr83/io.parsek)
[![codecov](https://codecov.io/gh/andr83/io.parsek/branch/master/graph/badge.svg)](https://codecov.io/gh/andr83/io.parsek)

Parsek is a Scala library for building ETL pipelines in functional way. 

## Overview
The main goal is to provide tools to work with data in generic form independently of source or target formats. For the initial idea was taken JSON AST and methods to work with it from libraries like [Circe](https://circe.github.io/circe/) and [Play Json](https://www.playframework.com/documentation/latest/ScalaJson). 

So why not original Circe and JSON? The main problem with JSON is limited type support. For example there missing important for ETL types like `Date`, `DateTime`, `Byte Array`. Also common tasks in ETL are data cleaning, validation and transforming from one form to another. Circe and especially Play will required a lot of boilerplate code.

Parsek has modular architecture with minimum external dependencies. Yes we know what is dependency hell! It explains why no dependency on [Scalaz](https://github.com/scalaz/scalaz)/[Cats](https://github.com/typelevel/cats) or [Monocle](http://julien-truffaut.github.io/Monocle/).

`Core` module focusing on AST, data encoding/decoding, schema definition and validation.

`Jackson` support JSON serialisation/deserialisation.

`Shapeless` for automatic generic types (case classes) derivation.

`JDBC` provide utilities to simplify communication with jdbc source.

## Quick start

```scala
val parsekVersion = "0.2.0"

// for >= Scala 2.10.6, 2.11.x, 2.12.x
libraryDependencies ++= Seq(
	"io.parsek" %% "parsek-core",
	"io.parsek" %% "parsek-jackson",
	"io.parsek" %% "parsek-shapeless",
	"io.parsek" %% "parsek-jdbc"			
).map(_ % parsekVersion)
```

In Scala REPL console:

```scala
import io.parsek._, io.parsek.implicits._
import io.parsek.shapeless.implicits._

case class Foo(x: Int, y: String)
// defined class Foo

val foo = Foo(42, "hello")
// foo: Foo = Foo(42,hello)

val pv = foo.toPValue
// converting case class to AST PValue representation
// io.parsek.PValue = PMap(Map('y -> PString(hello), 'x -> PInt(42)))

root.x.as[Int].modify(_ * 100)(pv)
// use lens with Dynamics support to modify PValue.
// res: io.parsek.PResult[io.parsek.PValue] = PSuccess(PMap(Map('y -> PString(hello), 'x -> PInt(4200))),List())

import io.parsek.optics.Projection
// import AST projection

val p = Projection(
 |     'x -> root.y.as[String],
 |     'z ->  root.x.as[Int],
 |     's -> Projection(
 |       'x -> root.x.as[Int],
 |       'y -> root.y.map[String, String](_.toUpperCase).as[String]
 |     )
 |   )
// create projection

val pv2 = p.get(pv).unsafe
// apply projection 
// pv2: io.parsek.PValue = PMap(Map('x -> PString(hello), 'z -> PInt(42), 's -> PMap(Map('x -> PInt(42), 'y -> PString(HELLO)))))

case class Bar(x: String, z: Int, s: Foo)
// defined class Bar

pv2.as[Bar]
// io.parsek.PResult[Bar] = PSuccess(Bar(hello,42,Foo(42,HELLO)),List())

import io.parsek.jackson._
// import JSON module

val serde = JsonSerDe()
// serde:io.parsek.jackson.JsonSerDe = JsonSerDe(com.fasterxml.jackson.databind.ObjectMapper@ace16b)

serde.write(pv2)
//res: String = {"x":"hello","z":42,"s":{"x":42,"y":"HELLO"}}

```

## License

MIT License

Copyright (c) 2018 Andrei Tupitcyn