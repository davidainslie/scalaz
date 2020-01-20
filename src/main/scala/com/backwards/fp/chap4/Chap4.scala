package com.backwards.fp.chap4

import java.util.regex.Pattern

object Adt extends App {
  // Data

  // The fundamental building blocks of data types are:
  // - final case class also known as products
  // - sealed abstract class also known as coproducts
  // - case object and Int, Double, String (etc) values
  // with no methods or fields other than the constructor parameters.
  // We prefer abstract class to trait in order to get better binary compatibility and to discourage trait mixing.
  // The collective name for products, coproducts and values is Algebraic Data Type (ADT).

  // We compose data types from the AND and XOR (exclusive OR) Boolean algebra:
  // a product contains every type that it is composed of,
  // but a coproduct can be only one. For example

  // product: ABC = a AND b AND c
  // coproduct: XYZ = x XOR y XOR z

  // Written in Scala:
  // values
  case object A
  type B = String
  type C = Int

  // product
  final case class ABC(a: A.type, b: B, c: C)

  // coproduct
  sealed abstract class XYZ
  case object X extends XYZ
  case object Y extends XYZ
  final case class Z(b: B) extends XYZ

  // Pattern matching ADT
  // Use sealed ADT along with scala compiler option "-Xfatal-warnings" to catch match issues at compile time
  sealed abstract class Foo
  final case class Bar(flag: Boolean) extends Foo
  final case object Baz extends Foo

  // So the following will not compile
  /*def thing(foo: Foo) = foo match {
    case Bar(_) => true
  }*/

  // However, the following will - watch out for this - don’t use guards on sealed types
  def thing(foo: Foo): Boolean = foo match {
    case Bar(flag) if flag => true
  }

  // Another form of product is a tuple, which is like an unlabelled final case class.

  // Another form of coproduct is when we nest Either types. e.g.
  Left(X): Either[X.type, Either[Y.type, Z]]
  // equivalent to the XYZ sealed abstract class.

  // and with cleaner syntax:
  type |:[L, R] = Either[L, R]

  Left(X): X.type |: Y.type |: Z

  // Constraints.
  // Don't do the following as exception can be thrown:
  /*
  final case class Person(name: String, age: Int) {
    require(name.nonEmpty && age > 0) // breaks Totality, don't do this!
  }
  */

  // Instead - smart constructor:
  final case class Person private(name: String, age: Int)

  object Person {
    def apply(name: String, age: Int): Either[String, Person] = {
      if (name.nonEmpty && age > 0) Right(new Person(name, age))
      else Left(s"bad input: $name, $age")
    }
  }

  def welcome(person: Person): String =
    s"${person.name} you look wonderful at ${person.age}!"

  val good = for {
    person <- Person("Bob", 99)
  } yield welcome(person)

  val bad = for {
    person <- Person("", -1)
  } yield welcome(person)

  println(good)
  println(bad)
}

object Refined extends App {
  import eu.timepit.refined
  import refined.api.Refined
  import refined.api.Validate
  import refined.numeric.Positive
  import refined.collection.NonEmpty
  import refined.refineV

  // With Refined, instead of a smart constructor we can:

  final case class Person(
    name: String Refined NonEmpty,
    age: Int Refined Positive
  )

  // We can construct a value at runtime using refineV, returning an Either:
  println(refineV[NonEmpty]("")) // Left(Predicate isEmpty() did not fail.)

  println(refineV[NonEmpty]("Sam")) // Right(Sam)

  // If we add the following import
  import refined.auto._
  // we can construct valid values at compiletime and get an error if the provided value does not meet the requirements:

  val sam: String Refined NonEmpty = "Sam"

  // Following will not compile
  // val empty: String Refined NonEmpty = ""

  import refined.W
  import refined.boolean.And
  import refined.collection.MaxSize

  // Constrain Person name to be non-empty and a maximum of 10 characters:
  type Name = NonEmpty And MaxSize[W.`10`.T]

  final case class PersonMoreConstrained(
    name: String Refined Name,
    age: Int Refined Positive
  )

  // The W notation is short for “witness”.
  // This syntax is much simpler in Scala 2.13, which has support for literal types e.g:

  // type Name = NonEmpty And MaxSize[10]

  // It is easy to define custom requirements that are not covered by the refined library.
  // For example ensure that a String contains application/x-www-form-urlencoded content.
  // We can create a Refined rule using the Java regular expression library:

  sealed abstract class UrlEncoded

  object UrlEncoded {
    private[this] val valid: Pattern =
      Pattern.compile("\\A(\\p{Alnum}++|[-.*_+=&]++|%\\p{XDigit}{2})*\\z")

    implicit def urlValidate: Validate.Plain[String, UrlEncoded] =
      Validate.fromPredicate(
        s => valid.matcher(s).find(),
        identity,
        new UrlEncoded {}
      )
  }
}

// By not providing any functionality, ADTs can have a minimal set of dependencies.
// This makes them easy to publish and share with other developers.
// By using a simple data modelling language, it makes it possible to interact with cross-discipline teams,
// such as DBAs, UI developers and business analysts, using the actual code instead of a hand written document as the source of truth.

// Furthermore, tooling can be more easily written to produce or consume schemas from other programming languages and wire protocols.

// Complexity:

// To find the complexity of a product, we multiply the complexity of each part:
// (Boolean, Boolean) has 4 values (2*2)
// (Boolean, Boolean, Boolean) has 8 values (2*2*2)

// To find the complexity of a coproduct, we add the complexity of each part:
// (Boolean |: Boolean) has 4 values (2+2)
// (Boolean |: Boolean |: Boolean) has 6 values (2+2+2)

// In FP, functions are total and must return an value for every input, no Exception.
// Minimising the complexity of inputs and outputs is the best way to achieve totality.
// As a rule of thumb, it is a sign of a badly designed function when the complexity of a function’s return value
// is larger than the product of its input.