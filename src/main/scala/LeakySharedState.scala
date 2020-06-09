import cats.effect.concurrent.Semaphore
import cats.effect.{ExitCode, IO, IOApp}
import cats._
import cats.implicits._
import scala.concurrent.duration._

class LaunchMissiles(val sem: Semaphore[IO]) {

 def run() : Unit = {

 }
}

object LeakySharedState extends IOApp {

  def putStrLn(s: String): IO[Unit] = IO(println(s))

  // global access
  val sem: Semaphore[IO] = Semaphore[IO](1).unsafeRunSync()

  def someExpensiveTask: IO[Unit] =
    IO.sleep(1.second) >> putStrLn("expensive task") >>  someExpensiveTask

  new LaunchMissiles(sem).run // Unit
  val p1: IO[Unit] = sem.withPermit(someExpensiveTask) >> p1

  val p2: IO[Unit] = sem.withPermit(someExpensiveTask) >> p2

  def run(args: List[String]): IO[ExitCode] =
    p1.start.void *> p2.start.void *> IO.never.as(ExitCode.Success)

}