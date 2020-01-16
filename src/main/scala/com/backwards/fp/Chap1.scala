package com.backwards.fp

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scalaz.Id.Id

object Chap1 extends App {
  {
    trait TerminalSync {
      def read(): String
      def write(t: String): Unit
    }

    trait TerminalAsync {
      def read(): Future[String]
      def write(t: String): Future[Unit]
    }

    // How do we write generic code that does something as simple as echo the user’s input
    // synchronously or asynchronously depending on our runtime implementation?

    // Higher Kinded Types allow us to use a type constructor in our type parameters, which looks like C[_].
    // This is a way of saying that whatever C is, it must take a type parameter. For example:

    trait Foo[C[_]] {
      def create(i: Int): C[Int]
    }

    // List is a type constructor because it takes a type (e.g. Int) and constructs a type (List[Int]).
    // We can implement Foo using List:

    object FooList extends Foo[List] {
      def create(i: Int): List[Int] = List(i)
    }
  }

  {
    // We want to define Terminal for a type constructor C[_].
    // By defining Now to construct to its type parameter (like Id),
    // we can implement a common interface for synchronous and asynchronous terminals:

    trait Terminal[C[_]] {
      def read: C[String]
      def write(t: String): C[Unit]
    }

    type Now[X] = X

    object TerminalSync extends Terminal[Now] {
      def read: String = scala.io.StdIn.readLine
      def write(t: String): Unit = println(t)
    }

    // or even

    object TerminalAlternativeSync extends Terminal[Id] {
      def read: String = ???
      def write(t: String): Unit = ???
    }

    class TerminalAsync(implicit EC: ExecutionContext) extends Terminal[Future] {
      // You could potentially implement these with the nio non-blocking API,
      // this implementation eats up a Thread for each operation.
      def read: Future[String]           = Future { scala.io.StdIn.readLine }
      def write(t: String): Future[Unit] = Future { println(t) }
    }

    // But we know nothing about C and we cannot do anything with a C[String].
    // What we need is a kind of execution environment:

    trait Execution[C[_]] {
      def chain[A, B](c: C[A])(f: A => C[B]): C[B]
      def create[B](b: B): C[B]
    }

    // This lets us write:

    def echo[C[_]](t: Terminal[C], e: Execution[C]): C[String] =
      e.chain(t.read) { in: String =>
        e.chain(t.write(in)) { _: Unit =>
          e.create(in)
        }
      }

    // We can write a mock implementation of Terminal[Now] and use it in our tests without any timeouts.
    // Implementations of Execution[Now] and Execution[Future] are reusable by generic methods like echo.

    // The implicit class Scala language feature gives C some methods:

    object Execution {
      implicit class Ops[A, C[_]](c: C[A]) {
        def flatMap[B](f: A => C[B])(implicit e: Execution[C]): C[B] =
          e.chain(c)(f)

        def map[B](f: A => B)(implicit e: Execution[C]): C[B] =
          e.chain(c)(f andThen e.create)
      }

      implicit val now: Execution[Now] = new Execution[Now] {
        def chain[A, B](c: A)(f: A => B): B = f(c)
        def create[B](b: B): B              = b
      }

      implicit def future(implicit EC: ExecutionContext): Execution[Future] =
        new Execution[Future] {
          def chain[A, B](c: Future[A])(f: A => Future[B]): Future[B] = c.flatMap(f)
          def create[B](b: B): Future[B] = Future.successful(b)
        }
    }

    import Execution.Ops

    def echo2[C[_]](implicit t: Terminal[C], e: Execution[C]): C[String] =
      t.read.flatMap { in: String =>
        t.write(in).map { _: Unit =>
          in
        }
      }

    // By using the name flatMap as the method name (instead of chain) we can use a for comprehension:

    def echo3[C[_]](implicit t: Terminal[C], e: Execution[C]): C[String] =
      for {
        in <- t.read
        _ <- t.write(in)
      } yield in

    // Our Execution has the same signature as a trait in Scalaz called Monad, except chain is bind and create is pure.
    // We say that C is monadic when there is an implicit Monad[C] available.

    // We might reasonably expect that calling any echo will not perform any side effects, because it is pure.
    // However, if we use Future or Id as the execution context, our application will start listening to stdin:

    implicit val now: Terminal[Now] = TerminalSync

    import scala.concurrent.ExecutionContext.Implicits.global
    implicit val future: Terminal[Future] = new TerminalAsync

    // Interpret for Now (impure)
    val nowEcho: Now[String] = echo3[Now]

    // Interpret for Future (impure)
    val futureEcho: Future[String] = echo3[Future]
    Await.result(futureEcho, Duration.Inf)

    // We have broken purity and are no longer writing FP code:
    // futureEcho is the result of running echo once.
    // Future conflates the definition of a program with interpreting it (running it).

    // Impure functions are not referentially transparent.
    // We cannot replace echo[Future] with a value, such as val futureEcho,
    // since the pesky user can type something different the second time.

    // We can define a simple safe F[_] execution context:

    final class IO[A](val interpret: () => A) {
      def map[B](f: A => B): IO[B] = IO(f(interpret()))
      def flatMap[B](f: A => IO[B]): IO[B] = IO(f(interpret()).interpret())
    }

    object IO {
      def apply[A](a: => A): IO[A] = new IO(() => a)
    }

    // which lazily evaluates a thunk.
    // IO is just a data structure that references (potentially) impure code, it isn’t actually running anything.
    // We can implement Terminal[IO]

    object TerminalIO extends Terminal[IO] {
      def read: IO[String]           = IO { scala.io.StdIn.readLine }
      def write(t: String): IO[Unit] = IO { println(t) }
    }

    implicit val io: Terminal[IO] = TerminalIO

    implicit val deferred: Execution[IO] = new Execution[IO] {
      def chain[A, B](c: IO[A])(f: A => IO[B]): IO[B] = c.flatMap(f)
      def create[B](b: B): IO[B]                      = IO(b)
    }

    val delayed: IO[String] = echo3[IO]

    // This val delayed can be reused, it is just the definition of the work to be done.
    // We can map the String and compose additional programs, much as we would map over a Future.

    // The impure code inside the IO is only evaluated when we .interpret() the value, which is an impure action.

    // Interpret, impure, end of the world
    delayed.interpret()

    // An application composed of IO programs is only interpreted once, in the main method,
    // which is also called the end of the world.
  }
}