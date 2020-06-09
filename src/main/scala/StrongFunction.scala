import cats.Functor
import cats.data.State
import cats.effect.IO

import scala.util.control.NoStackTrace

object StrongFunction {

  import cats.implicits._

  case class User(name: String)

  case class Username private(val value: String) extends AnyVal

  case class Email private(val value: String) extends AnyVal

  //  sealed abstract class UsernameV1(value: String)
  //  sealed abstract class EmailV1(value: String)

  import io.estatico.newtype.macros._

//  @newtype case class Username(value: String)
//
//  @newtype case class Email(value: String)

  // Smart constructors
  def mkUsername(value: String): Option[Username] =
    if (value.nonEmpty) Username(value).some
    else none[Username]

  def mkEmail(value: String): Option[Email] =
    if (value.contains("@")) Email(value).some
    else none[Email]


  def lookup[F[_]](username: Username, email: Email): F[Option[User]] = ???

  def lookupV1 = (
    mkUsername("aeinstein"),
    mkEmail("aeinstein@research.com")
    ).mapN {
    case (username, email) => lookup(username, email)
  }

  import eu.timepit.refined._
  import eu.timepit.refined.auto._
  import eu.timepit.refined.types.string.NonEmptyString

  import eu.timepit.refined.api.Refined
  import eu.timepit.refined.collection.Contains

  type UsernameFull = String Refined Contains['g']

  def lookupV2[F[_]](username: UsernameFull): F[Option[User]] = ???

  trait Counter[F[_]] {
    def incr: F[Unit]

    def get: F[Int]
  }

  import cats.effect.concurrent.Ref

  // Make contstuctor private to protect state from leak
//  class LiveCounter[F[_]] private(ref: Ref[F, Int]) extends Counter[F] {
//    def incr: F[Unit] = ref.update(_ + 1)
//    def get: F[Int] = ref.get
//  }

  import cats.effect.Sync
  import cats.implicits._

  object LiveCounter {
    def make[F[_]: Sync]: F[Counter[F]] =
      Ref.of[F, Int](0).map { ref => new Counter[F] {
        def incr: F[Unit] = ref.update(_ + 1)
        def get: F[Int] = ref.get }
      }
  }

  def program(counter: Counter[IO]): IO[Unit] =
    counter.incr

  val nextInt: State[Int, Int] = State(s => (s + 1, s * 2))

  def seq: State[Int, Int] = for {
    n1 <- nextInt
    n2 <- nextInt
    n3 <- nextInt
  } yield n1 + n2 + n3

  sealed trait BusinessError extends NoStackTrace
  case object RandomError extends BusinessError

  trait Categories[F[_]] {
    def maybeFindAll: F[Either[RandomError.type, List[Category]]]
  }

  case class Category(name: String) extends AnyVal

//  class Program[F[_]: Functor]( categories: Categories[F]
//                              ){
//    def findAll: F[List[Category]] = category.maybeFindAll.map {
//      case Right(c) => c
//      case Left(RandomError) => List.empty[Category] }
//  }

  def main(args: Array[String]): Unit = {

    val user = mkUsername("Fox")
    println(user)

    Email("foo")

    import io.estatico.newtype.ops._

    // anti-pattern
    //val email = "foo".coerce[Email]
    //println(email)

    //lookupV2("FoxFog")

    //val counter = LiveCounter.make[IO[Int]]
    //println(counter)

    val res = seq.run(1)
    println( res.value )

  }

}
