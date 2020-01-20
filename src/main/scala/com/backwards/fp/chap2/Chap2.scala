package com.backwards.fp.chap2

object Chap2 extends App {
  // The following API is not good.
  // There are many functions in the wild that take Option parameters but actually require all parameters to exist:

  def namedThingsBad(
    someName  : Option[String],
    someNumber: Option[Int]
  ): Option[String] = for {
    name   <- someName
    number <- someNumber
  } yield s"$number ${name}s"

  // If a function requires every input then it should make its requirement explicit,
  // pushing the responsibility of dealing with optional parameters to its caller:
  def namedThings(name: String, num: Int) = s"$num ${name}s"
}