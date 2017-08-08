package io.parsek

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import java.time.temporal.{TemporalAccessor, TemporalQuery}

/**
  * @author andr83 
  * todo: Refactor to PValueFormatter
  */
trait InstantFormatter {
  def format(time: Instant): Long Either String

  def parse(value: String): Instant

  def parse(value: Long): Instant = Instant.ofEpochMilli(value)
}

object InstantFormatter {
  def apply(): InstantFormatter = new InstantFormatter {
    override def format(time: Instant): Either[Long, String] = Left(time.toEpochMilli)

    override def parse(value: String): Instant = Instant.parse(value)
  }

  def apply(pattern: String): InstantFormatter = apply(DateTimeFormatter.ofPattern(pattern).withZone(ZoneId.systemDefault()))

  def apply(formatter: DateTimeFormatter): InstantFormatter = new InstantFormatter {
    override def format(time: Instant): Either[Long, String] = Right(formatter.format(time))

    override def parse(value: String): Instant = formatter.parse(value, new TemporalQuery[Instant] {
      override def queryFrom(temporal: TemporalAccessor): Instant = Instant.from(temporal)
    })
  }
}
