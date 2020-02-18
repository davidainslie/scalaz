package com.backwards.fp.chap5

import java.time.LocalDate
import scalaz.{Apply, Monoid}
import simulacrum.typeclass

/**
 * <img src="scalaz-hierarchy.png"/>
 */
object CategoryTheory extends App {
  // The four most important methods from a control flow perspective

  // Typeclass	    Method	    From	    Given	      To

  // Functor	      map	        F[A]	    A => B	    F[B]

  // Applicative	  pure	      A	 	                  F[A]

  // Monad	        flatMap	    F[A]	    A => F[B]	  F[B]

  // Traverse	      sequence	  F[G[A]]	 	            G[F[A]]


  // Operations which return a F[_] can be run sequentially in a for comprehension by .flatMap, defined on its Monad[F].
  // The context F[_] can be thought of as a container for an intentional effect.
}

object AppendableThings extends App {
  import scalaz.std.string._
  import scalaz.syntax.semigroup._

  val x = "hello" |+| " " |+| "world!"
  println(x)
}

object MonoidApp extends App {
  import scalaz.std.list._
  // import scalaz.std.option._
  import scalaz.syntax.semigroup._

  // As a realistic example for Monoid, consider a trading system that has a large database of reusable trade templates.
  // Populating the default values for a new trade involves selecting and combining multiple templates,
  // with a “last rule wins” merge policy if two templates provide a value for the same field.

  sealed abstract class Currency

  case object EUR extends Currency

  case object USD extends Currency

  final case class TradeTemplate(payments: List[LocalDate], ccy: Option[Currency], otc: Option[Boolean])

  object TradeTemplate {
    implicit def lastWins[A]: Monoid[Option[A]] = Monoid.instance(
      {
        case (None, None)   => None
        case (only, None)   => only
        case (None, only)   => only
        case (_   , winner) => winner
      },
      None
    )

    implicit val monoid: Monoid[TradeTemplate] = Monoid.instance(
      (a, b) => TradeTemplate(a.payments |+| b.payments, a.ccy |+| b.ccy, a.otc |+| b.otc),
      TradeTemplate(Nil, None, None)
    )
  }

  val zero = Monoid[TradeTemplate].zero

  val templates = List(
    TradeTemplate(Nil,                            None,      None),
    TradeTemplate(Nil,                            Some(EUR), None),
    TradeTemplate(List(LocalDate.of(2017, 8, 5)), Some(USD), None),
    TradeTemplate(List(LocalDate.of(2017, 9, 5)), None,      Some(true)),
    TradeTemplate(Nil,                            None,      Some(false))
  )

  val result = templates.foldLeft(zero)(_ |+| _)
  // result: TradeTemplate = TradeTemplate(List(2017-08-05, 2017-09-05), Some(USD), Some(false))
  println(result)

  // Note that the list of payments are concatenated.
  // This is because the default Monoid[List] uses concatenation of elements and happens to be the desired behaviour.
  // If the business requirement was different, it would be a simple case of providing a custom Monoid[List[LocalDate]].
}

object FunctorApp extends App {
  // Scalaz declares the Functor typeclass along the lines of:
  @typeclass trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    // Takes an instance of the F[A] and always returns an F[Unit], it forgets all the values whilst preserving the structure.
    def void[A](fa: F[A]): F[Unit] = map(fa)(_ => ())

    // Takes the same input as map but returns F[(A, B)], i.e. it tuples the contents with the result of applying the function.
    // This is useful when we wish to retain the input.
    def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => (a, f(a)))

    // Twins all the elements of A into a tuple F[(A, A)].
    def fpair[A](fa: F[A]): F[(A, A)] = map(fa)(a => (a, a))

    // Pairs the contents of an F[B] with a constant A on the left.
    def strengthL[A, B](a: A, f: F[B]): F[(A, B)] = map(f)(b => (a, b))

    // Pairs the contents of an F[A] with a constant B on the right.
    def strengthR[A, B](f: F[A], b: B): F[(A, B)] = map(f)(a => (a, b))

    // Takes a function A => B and returns a F[A] => F[B].
    // In other words, it takes a function over the contents of an F[A] and returns a function that operates on the F[A] directly.
    def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)

    // Say we have an F[_] of functions A => B and a value A, then we can get an F[B].
    // It has a similar signature to pure but requires the caller to provide the F[A => B].
    def mapply[A, B](a: A)(f: F[A => B]): F[B] = map(f)((ff: A => B) => ff(a))
  }
}

object InvariantFunctor extends App {
  //                                         InvariantFunctor
  //                                         ^               ^
  //                                        /                 \
  //                                       /                   \
  //                                      /                     \
  //                                Contravariant             Functor
  // More specifically called    (Contravariant Functor)   (Covariant Functor)

  import jsonformat._

  final case class Alpha(value: Double)

  // JsEncoder has a Contravariant and JsDecoder has a Functor, so we can derive instances.

  object Alpha {
    // If you give me a JsDecoder for a Double, and a way to go from a Double to an Alpha, then I can give you a JsDecoder for an Alpha
    implicit val decoder: JsDecoder[Alpha] = JsDecoder[Double].map(Alpha(_))

    // If you give me a JsEncoder for a Double, and a way to go from an Alpha to a Double, then I can give you a JsEncoder for an Alpha
    implicit val encoder: JsEncoder[Alpha] = JsEncoder[Double].contramap(_.value)
  }
}

object Apply extends App {
  // Apply extends Functor by adding a method named ap which is similar to map in that it applies a function to values.
  // However, with ap, the function is in the same context as the values.
  //
  // @typeclass trait Apply[F[_]] extends Functor[F] {
  //   @op("<*>") def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B]
  // }

  // For example
  implicit def option[T]: Apply[Option[T]] = new Apply[Option[T]] {
    override def ap[A, B](fa: => Option[A])(f: => Option[A => B]): Option[B] = f match {
      case None => None
      case Some(ff) => fa.map(ff)
    }

    def map[A, B](fa: Option[A])(f: A => B): Option[B] = ???
  }

  // To implement .ap, we must first extract the function ff: A => B from f: Option[A => B], then we can map over fa.
  // The extraction of the function from the context is the important power that Apply brings, allowing multiple function to be combined inside the context.

  // Returning to Apply, we find .applyX boilerplate that allows us to combine parallel functions and then map over their combined output:
  // @typeclass trait Apply[F[_]] extends Functor[F] {
  //   def ap2[A, B, C](fa: => F[A], fb: => F[B])(f: F[(A, B) => C]): F[C] =
  //     ap(fb)(ap(fa)(map(f)(_.curried)))
  //
  //   def ap3[A, B, C, D](fa: => F[A], fb: => F[B], fc: => F[C])(f: F[(A, B, C) => D]): F[D] =
  //     ap(fc)(ap2(fa, fb)(map(f)(f => ((a: A, b: B) => (c: C) => f(a, b, c)))))

  // Read .ap2 as a contract promising:
  // "if you give me an F of A and an F of B, with a way of combining A and B into a C, then I can give you an F of C".
  //
  // There are many uses for this contract and the two most important are:
  // - constructing some typeclasses for a product type C from its constituents A and B
  // - performing effects in parallel

  // And before we introduce Monad, let's compare Monad with Applicative:

  // for {
  //   foo <- data.foo: Option[String]
  //   bar <- data.bar: Option[Int]
  // } yield foo + bar.shows

  // (data.foo |@| data.bar)(_ + _.shows)
}