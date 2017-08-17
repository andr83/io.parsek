package io.parsek

import java.time.{Instant, LocalDate, ZoneId}
import java.time.format.DateTimeFormatter
import java.time.temporal.{TemporalAccessor, TemporalQuery}

/**
  * @author andr83
  */
trait PValueFormatter {
  def formatInstant(time: Instant): Long Either String

  def formatDate(date: LocalDate): String

  def parseInstant(value: String): Instant

  def parseInstant(value: Long): Instant = Instant.ofEpochMilli(value)

  def parseDate(value: String): LocalDate
}

object PValueFormatter {
  def apply(): PValueFormatter = new PValueFormatter {
    override def formatInstant(time: Instant): Either[Long, String] = Left(time.toEpochMilli)

    override def parseInstant(value: String): Instant = Instant.parse(value)

    override def formatDate(date: LocalDate): String = DateTimeFormatter.ISO_LOCAL_DATE.format(date)

    override def parseDate(value: String): LocalDate = LocalDate.from(DateTimeFormatter.ISO_LOCAL_DATE.parse(value))
  }

  def apply(dateTimePattern: String, datePattern: String): PValueFormatter =
    apply(DateTimeFormatter.ofPattern(dateTimePattern).withZone(ZoneId.systemDefault()),
      DateTimeFormatter.ofPattern(datePattern).withZone(ZoneId.systemDefault()))

  def apply(dateTimeFormatter: DateTimeFormatter, dateFormatter: DateTimeFormatter): PValueFormatter = new PValueFormatter {
    override def formatInstant(time: Instant): Either[Long, String] = Right(dateTimeFormatter.format(time))

    override def parseInstant(value: String): Instant = dateTimeFormatter.parse(value, new TemporalQuery[Instant] {
      override def queryFrom(temporal: TemporalAccessor): Instant = Instant.from(temporal)
    })

    override def formatDate(date: LocalDate): String = dateFormatter.format(date)

    override def parseDate(value: String): LocalDate = LocalDate.from(dateFormatter.parse(value))
  }
}
