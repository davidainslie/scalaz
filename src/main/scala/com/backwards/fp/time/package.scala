package com.backwards.fp

import java.time.Instant
import scala.util.control.NonFatal
import contextual.{Context, Prefix, Verifier}

package object time {
  implicit class EpochMillisStringContext(sc: StringContext) {
    val epoch: Prefix[Epoch, Context, EpochInterpolator.type] =
      Prefix(EpochInterpolator, sc)
  }

  object EpochInterpolator extends Verifier[Epoch] {
    def check(s: String): Either[(Int, String), Epoch] =
      try {
        Right(Epoch(Instant.parse(s).toEpochMilli))
      } catch { case NonFatal(_) =>
        Left((0, "not in ISO-8601 format"))
      }
  }
}