package function

package object iomonad {
  type IO[A] = IO3.IO[A]
  def IO[A](a: => A): IO[A] = IO3.IO[A](a)
}
