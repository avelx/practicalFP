import cats.effect._
import cats.effect.concurrent.Semaphore
import cats.effect.implicits._
import cats.implicits._

import scala.concurrent.duration._
import scala.util.Random

object SharedState extends IOApp {

  def putStrLn(s: String): IO[Unit] = IO(println(s))

  def someExpensiveTask(marker: String): IO[Unit] =
    IO.sleep( Random.nextInt(10).second ) >> putStrLn(s"expensive task: $marker") >> someExpensiveTask(marker)

  def p1(sem: Semaphore[IO]): IO[Unit] =
    sem.withPermit(someExpensiveTask("Task A")) >> p1(sem)

  def p2(sem: Semaphore[IO]): IO[Unit] =
    sem.withPermit(someExpensiveTask("Task B")) >> p2(sem)

  // Locking region of sharing for Sem
  def run(args: List[String]): IO[ExitCode] = {
    Semaphore[IO](2).flatMap { sem =>
      p1(sem).start.void *> p2(sem).start.void
    } *> IO.never.as(ExitCode.Success)
  }

}