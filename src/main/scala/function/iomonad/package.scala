package function

import java.util.concurrent.ExecutorService

package object iomonad {

  import function.parallelism.Nonblocking._

  type IO[A] = IO3.IO[A]

  def IO[A](a: => A): IO[A] = IO3.IO[A](a)

  def par[A](a: Par[A]): IO[A] = IO3.Suspend(a)

  def Return[A](a: A): IO[A] = IO3.Return[Par, A](a)


  def unsafePerformIO[A](io: IO[A])(implicit E: ExecutorService): A = Par.run(E) {
    IO3.run(io)(IO3.parMonad)
  }

}
