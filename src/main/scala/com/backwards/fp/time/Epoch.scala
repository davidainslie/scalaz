package com.backwards.fp.time

import java.time.Instant
import scala.concurrent.duration.FiniteDuration
import scalaz.{Order, Show}
import scalaz.ioeffect.IO
import pureconfig.ConfigReader
import scala.concurrent.duration._
import scalaz.annotation.xderiving
import pureconfig.error.FailureReason

// @xderiving(Order, Arbitrary)
final case class Epoch(millis: Long) extends AnyVal {
  def +(d: FiniteDuration): Epoch = Epoch(millis + d.toMillis)
  def -(e: FiniteDuration): Epoch = Epoch(millis - e.toMillis)
  def -(e: Epoch): FiniteDuration = (millis - e.millis).millis

  implicit def DurationLong(n: Long): DurationLong =
    new DurationLong(n)
}

object Epoch {
  def now: IO[Void, Epoch] =
    IO.sync(Epoch(System.currentTimeMillis)) // scalafix:ok

  implicit val show: Show[Epoch] =
    Show.shows(e => Instant.ofEpochMilli(e.millis).toString) // scalafix:ok

  implicit val configReader: ConfigReader[Epoch] =
    ConfigReader[String].emap(
      s =>
        EpochInterpolator.check(s) match {
          case Left((_, err)) => failureReason(err)
          case Right(success) => Right(success)
        }
    )

  def failureReason[A](msg: String): Either[FailureReason, A] =
    Left(
      new FailureReason {
        def description: String = msg
      }
    )
}