package com.backwards.fp.chap4

import java.net.URI
import java.util.regex.Pattern
import scalaz.{IList, Monad, \/}
import scalaz.\/._
import com.backwards.fp.time.Epoch

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

object RefinedApp extends App {
  import eu.timepit.refined.api.Refined
  import eu.timepit.refined.api.Validate
  import eu.timepit.refined.numeric.Positive
  import eu.timepit.refined.collection.NonEmpty
  import eu.timepit.refined.refineV

  // With Refined, instead of a smart constructor we can:

  final case class Person(
    name: String Refined NonEmpty,
    age: Int Refined Positive
  )

  // We can construct a value at runtime using refineV, returning an Either:
  println(refineV[NonEmpty]("")) // Left(Predicate isEmpty() did not fail.)

  println(refineV[NonEmpty]("Sam")) // Right(Sam)

  // If we add the following import
  import eu.timepit.refined.auto._
  // we can construct valid values at compiletime and get an error if the provided value does not meet the requirements:

  val sam: String Refined NonEmpty = "Sam"

  // Following will not compile
  // val empty: String Refined NonEmpty = ""

  import eu.timepit.refined.W
  import eu.timepit.refined.boolean.And
  import eu.timepit.refined.collection.MaxSize

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
}

object TypeClass extends App {
  // The most common kind of function is a polymorphic function, which lives in a typeclass.
  // A typeclass is a trait that:
  // - holds no state
  // - has a type parameter
  // - has at least one abstract method (primitive combinators)
  // - may contain generalised methods (derived combinators)
  // - may extend other typeclasses

  def signOfTheTimes[T](t: T)(implicit N: Numeric[T]): T = {
    import N._
    times(negate(abs(t)), t)
  }

  // Note: We are no longer dependent on the OOP hierarchy of our input types,
  // i.e. we don't demand that our input "is a" Numeric, which is vitally important
  // if we want to support a third party class that we cannot redefine.

  // Another advantage of typeclasses is that the association of functionality to data is at compiletime,
  // as opposed to OOP runtime dynamic dispatch.
}

object Simulacrum extends App {
  // First step towards Simulacrum is to reduce/understand the boilerplate above,
  // by introducing ops on the typeclass companion:

  object Numeric {
    def apply[T](implicit numeric: Numeric[T]): Numeric[T] = numeric

    object ops {
      implicit class NumericOps[T](t: T)(implicit N: Numeric[T]) {
        def +(o: T): T = N.plus(t, o)
        def *(o: T): T = N.times(t, o)
        def unary_- : T = N.negate(t)
        def abs: T = N.abs(t)

        // Duplicated from Ordering.ops
        def <(o: T): Boolean = N.lt(t, o)
        def >(o: T): Boolean = N.gt(t, o)
      }
    }
  }

  // Now we can write:
  import Numeric.ops._

  def signOfTheTimes[T: Numeric](t: T): T = -t.abs * t

  // But, we never need to write this boilerplate because Simulacrum provides a
  // @typeclass macro annotation that automatically generates the "apply" and "ops".
}

object UsingSimulacrum extends App {
  import simulacrum._

  // TODO - Macro expansion issue needs this silly wrapper
  object Foo {
    @typeclass
    trait Ordering[T] {
      def compare(x: T, y: T): Int
      @op("<") def lt(x: T, y: T): Boolean = compare(x, y) < 0
      @op(">") def gt(x: T, y: T): Boolean = compare(x, y) > 0
    }

    @typeclass
    trait Numeric[T] extends Ordering[T] {
      @op("+") def plus(x: T, y: T): T
      @op("*") def times(x: T, y: T): T
      @op("unary_-") def negate(x: T): T
      def zero: T
      def abs(x: T): T = if (lt(x, zero)) negate(x) else x
    }
  }

  import Foo.Numeric.ops

  // TODO - Broken for some reason
  // def signOfTheTimes[T: Foo.Numeric](t: T): T = -t.abs * t
}

object ImplicitResolution extends App {
  // The normal variable scope is searched for implicits, in order:
  // - local scope, including scoped imports (e.g. the block or method)
  // - outer scope, including scoped imports (e.g. members in the class)
  // - ancestors (e.g. members in the super class)
  // - the current package object
  // - ancestor package objects (when using nested packages)
  // - the file’s imports

  // Implicits are often defined on a trait, which is then extended by an object.
  // This is to try and control the priority of an implicit relative to another more specific one, to avoid ambiguous implicits.
}

object OAuth2 extends App {
  // Every Google Cloud application needs to have an OAuth 2.0 Client Key set up at:
  // https://console.developers.google.com/apis/credentials?project={PROJECT_ID}
  // Obtaining a Client ID and a Client secret.

  import eu.timepit.refined.api.Refined
  import eu.timepit.refined.string.Url

  // The application can then obtain a one time code by making the user perform an Authorization Request in their browser.
  // https://accounts.google.com/o/oauth2/v2/auth?\
  //    redirect_uri={CALLBACK_URI}&\
  //    prompt=consent&\
  //    response_type=code&\
  //    scope={SCOPE}&\
  //    access_type=offline&\
  //    client_id={CLIENT_ID}
  final case class AuthRequest(
    redirect_uri: String Refined Url,
    scope: String,
    client_id: String,
    prompt: String = "consent",
    response_type: String = "code",
    access_type: String = "offline"
  )
  // The code is delivered to the {CALLBACK_URI} in a GET request. To capture it in our application, we need to have a web server listening on localhost.

  // Once we have the code, we can perform an Access Token Request.
  // POST /oauth2/v4/token HTTP/1.1
  //    Host: www.googleapis.com
  //    Content-length: {CONTENT_LENGTH}
  //    content-type: application/x-www-form-urlencoded
  //    user-agent: google-oauth-playground
  //    code={CODE}&\
  //    redirect_uri={CALLBACK_URI}&\
  //    client_id={CLIENT_ID}&\
  //    client_secret={CLIENT_SECRET}&\
  //    scope={SCOPE}&\
  //    grant_type=authorization_code
  final case class AccessRequest(
    code: String,
    redirect_uri: String Refined Url,
    client_id: String,
    client_secret: String,
    scope: String = "",
    grant_type: String = "authorization_code"
  )

  // which gives a JSON response payload
  // {
  //    "access_token": "BEARER_TOKEN",
  //    "token_type": "Bearer",
  //    "expires_in": 3600,
  //    "refresh_token": "REFRESH_TOKEN"
  // }
  final case class AccessResponse(
    access_token: String,
    token_type: String,
    expires_in: Long,
    refresh_token: String
  )

  // Bearer tokens typically expire after an hour, and can be refreshed by sending an HTTP request with any valid refresh token.
  // POST /oauth2/v4/token HTTP/1.1
  //    Host: www.googleapis.com
  //    Content-length: {CONTENT_LENGTH}
  //    content-type: application/x-www-form-urlencoded
  //    user-agent: google-oauth-playground
  //    client_secret={CLIENT_SECRET}&
  //    grant_type=refresh_token&
  //    refresh_token={REFRESH_TOKEN}&
  //    client_id={CLIENT_ID}
  final case class RefreshRequest(
    client_secret: String,
    refresh_token: String,
    client_id: String,
    grant_type: String = "refresh_token"
  )

  // responding with
  // {
  //    "access_token": "BEARER_TOKEN",
  //    "token_type": "Bearer",
  //    "expires_in": 3600
  // }
  final case class RefreshResponse(
    access_token: String,
    token_type: String,
    expires_in: Long
  )

  // All userland requests to the server should include the header:
  // Authorization: Bearer BEARER_TOKEN
  // after substituting the actual BEARER_TOKEN.

  // WARNING
  // Avoid using java.net.URL at all costs: it uses DNS to resolve the hostname part when performing toString, equals or hashCode.
  // Apart from being insane, and very very slow, these methods can throw I/O exceptions (are not pure), and can change depending on the network configuration (are not deterministic).

  // We need to marshal these data classes into JSON, URLs and POST-encoded forms.
  // Since this requires polymorphism, we will need typeclasses, where we can use the module jsonformat

  import jsonformat._

  implicit class JsValueOps(j: JsValue) {
    def getAs[A: JsDecoder](key: String): String \/ A = j match {
      // TODO - Rubbish
      case JsObject(xs) => xs.collectFirst {
        case (`key`, j) => j
      }.fold(left[String, A]("Whoops"))( j => j.getAs[A](key))

      case j => implicitly[JsDecoder[A]].fromJson(j)
    }
  }

  object AccessResponse {
    implicit val json: JsDecoder[AccessResponse] = j =>
      for {
        acc <- j.getAs[String]("access_token")
        tpe <- j.getAs[String]("token_type")
        exp <- j.getAs[Long]("expires_in")
        ref <- j.getAs[String]("refresh_token")
      } yield AccessResponse(acc, tpe, exp, ref)
  }

  object RefreshResponse {
    implicit val json: JsDecoder[RefreshResponse] = j =>
      for {
        acc <- j.getAs[String]("access_token")
        tpe <- j.getAs[String]("token_type")
        exp <- j.getAs[Long]("expires_in")
      } yield RefreshResponse(acc, tpe, exp)
  }

  import jsonformat.JsDecoder.ops._

  val json: String \/ JsValue = JsParser("""{
    "access_token": "BEARER_TOKEN",
    "token_type": "Bearer",
    "expires_in": 3600,
    "refresh_token": "REFRESH_TOKEN"
  }""")

  val accessResponse: String \/ AccessResponse = json.flatMap(_.as[AccessResponse])
  println(accessResponse)

  // We need to provide typeclass instances for basic types:
  import java.net.URLEncoder
  import simulacrum.typeclass
  import eu.timepit.refined.api.Refined
  import com.backwards.fp.chap4.RefinedApp.UrlEncoded

  @typeclass trait UrlEncodedWriter[A] {
    def toUrlEncoded(a: A): String Refined UrlEncoded
  }

  object UrlEncodedWriter {
    import ops._

    // Prior to SAM types, a common pattern was to define a method named instance on the typeclass companion.
    // It is being used here because of: no SAM here https://github.com/scala/bug/issues/10814
    def instance[T](f: T => String Refined UrlEncoded): UrlEncodedWriter[T] =
      new UrlEncodedWriter[T] {
        override def toUrlEncoded(t: T): String Refined UrlEncoded = f(t)
      }

    implicit val encoded: UrlEncodedWriter[String Refined UrlEncoded] =
      instance(identity)

    implicit val string: UrlEncodedWriter[String] =
      instance(s => Refined.unsafeApply(URLEncoder.encode(s, "UTF-8")))

    implicit val url: UrlEncodedWriter[String Refined Url] =
      instance(s => s.value.toUrlEncoded)

    implicit val long: UrlEncodedWriter[Long] =
      instance(l => Refined.unsafeApply(l.toString))

    import scalaz.Scalaz._ // For intercalate

    implicit def ilist[K: UrlEncodedWriter, V: UrlEncodedWriter]: UrlEncodedWriter[IList[(K, V)]] = instance { l =>
      val raw = l.map {
        case (k, v) => k.toUrlEncoded.value + "=" + v.toUrlEncoded.value
      }.intercalate("&")

      Refined.unsafeApply(raw) // By deduction
    }
  }

  // URL query key=value pairs, in unencoded form.
  final case class UrlQuery(params: IList[(String, String)]) extends AnyVal

  object UrlQuery {
    import scalaz.Scalaz._ // For intercalate

    object ops {
      implicit class UrlOps(private val encoded: String Refined Url) {
        def withQuery(query: UrlQuery): String Refined Url = {
          val uri = new URI(encoded.value)

          val update = new URI(
            uri.getScheme,
            uri.getUserInfo,
            uri.getHost,
            uri.getPort,
            uri.getPath,
            // Not a mistake: URI takes the decoded versions
            query.params.map { case (k, v) => k + "=" + v }.intercalate("&"),
            uri.getFragment
          )

          Refined.unsafeApply(update.toASCIIString)
        }
      }
    }
  }

  @typeclass trait UrlQueryWriter[A] {
    def toUrlQuery(a: A): UrlQuery
  }

  object AuthRequest {
    implicit val query: UrlQueryWriter[AuthRequest] = { a =>
      UrlQuery(
        IList(
          "redirect_uri"  -> a.redirect_uri.value,
          "scope"         -> a.scope,
          "client_id"     -> a.client_id,
          "prompt"        -> a.prompt,
          "response_type" -> a.response_type,
          "access_type"   -> a.access_type
        )
      )
    }
  }

  object AccessRequest {
    import UrlEncodedWriter.ops._

    implicit val encoded: UrlEncodedWriter[AccessRequest] = { a =>
      IList(
        "code"          -> a.code.toUrlEncoded,
        "redirect_uri"  -> a.redirect_uri.toUrlEncoded,
        "client_id"     -> a.client_id.toUrlEncoded,
        "client_secret" -> a.client_secret.toUrlEncoded,
        "scope"         -> a.scope.toUrlEncoded,
        "grant_type"    -> a.grant_type.toUrlEncoded
      ).toUrlEncoded
    }
  }

  object RefreshRequest {
    import UrlEncodedWriter.ops._

    implicit val encoded: UrlEncodedWriter[RefreshRequest] = { r =>
      IList(
        "client_secret" -> r.client_secret.toUrlEncoded,
        "refresh_token" -> r.refresh_token.toUrlEncoded,
        "client_id"     -> r.client_id.toUrlEncoded,
        "grant_type"    -> r.grant_type.toUrlEncoded
      ).toUrlEncoded
    }
  }

  // We define our dependency algebras, and use context bounds to show that our responses must have a JsDecoder and our POST payload must have a UrlEncodedWriter:
  trait JsonClient[F[_]] {
    def get[A: JsDecoder](
      uri: String Refined Url,
      headers: IList[(String, String)]
    ): F[A]

    def post[P: UrlEncodedWriter, A: JsDecoder](
      uri: String Refined Url,
      payload: P,
      headers: IList[(String, String)] = IList.empty
    ): F[A]
  }

  // Obtaining a CodeToken from the Google OAuth2 server involves:
  // 1. Starting an HTTP server on the local machine, and obtaining its port number.
  // 2. Making the user open a web page in their browser, which allows them to log in with their Google credentials and authorise the application, with a redirect back to the local machine.
  // 3. capturing the code, informing the user of next steps, and closing the HTTP server.

  // We can model this with three methods on a UserInteraction algebra:
  final case class CodeToken(token: String, redirect_uri: String Refined Url)

  trait UserInteraction[F[_]] {
    def start: F[String Refined Url]
    def open(uri: String Refined Url): F[Unit]
    def stop: F[CodeToken]
  }

  // We also need an algebra to abstract over the local system time:
  trait LocalClock[F[_]] {
    def now: F[Epoch]
  }

  // Data types that we will use in the refresh logic
  final case class ServerConfig(
    auth: String Refined Url,
    access: String Refined Url,
    refresh: String Refined Url,
    scope: String,
    clientId: String,
    clientSecret: String
  )

  final case class RefreshToken(token: String)

  final case class BearerToken(token: String, expires: Epoch)

  // Now we can write an OAuth2 client module:
  import scala.concurrent.duration._
  import scalaz.Scalaz._
  import UrlQueryWriter.ops._
  import UrlQuery.ops._

  class OAuth2Client[F[_]: Monad](
    config: ServerConfig
  )(
    user: UserInteraction[F],
    client: JsonClient[F],
    clock: LocalClock[F]
  ) {
    def authenticate: F[CodeToken] =
      for {
        callback <- user.start
        params   = AuthRequest(callback, config.scope, config.clientId)
        _        <- user.open(config.auth.withQuery(params.toUrlQuery))
        code     <- user.stop
      } yield code

    def access(code: CodeToken): F[(RefreshToken, BearerToken)] =
      for {
        request <- AccessRequest(code.token, code.redirect_uri, config.clientId, config.clientSecret).pure[F]
        msg     <- client.post[AccessRequest, AccessResponse](config.access, request)
        time    <- clock.now
        expires = time + msg.expires_in.seconds
        refresh = RefreshToken(msg.refresh_token)
        bearer  = BearerToken(msg.access_token, expires)
      } yield (refresh, bearer)

    def bearer(refresh: RefreshToken): F[BearerToken] =
      for {
        request <- RefreshRequest(config.clientSecret, refresh.token, config.clientId).pure[F]
        msg     <- client.post[RefreshRequest, RefreshResponse](config.refresh, request)
        time    <- clock.now
        expires = time + msg.expires_in.seconds
        bearer  = BearerToken(msg.access_token, expires)
      } yield bearer
  }

  // Summary

  // - Algebraic data types (ADTs) are defined as products (final case class) and coproducts (sealed abstract class).

  // - Refined types enforce constraints on values.

  // - Concrete functions can be defined in an implicit class to maintain left-to-right flow.

  // - Polymorphic functions are defined in typeclasses. Functionality is provided via “has a” context bounds, rather than “is a” class hierarchies.

  // - Typeclass instances are implementations of a typeclass.

  // - @simulacrum.typeclass generates .ops on the companion, providing convenient syntax for typeclass functions.

  // - Typeclass derivation is compiletime composition of typeclass instances.
}