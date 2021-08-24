package function.iomonad

/**
 * 描述：
 *
 * @author yuli
 * @date 2021/8/24
 */
case class Task[A](get: IO[Either[Throwable, A]]) {
  def flatMap[B](f: A => Task[B]): Task[B] = Task(get flatMap {
    case Left(e) => IO(Left(e))
    case Right(a) => Right(f(a))
  })

  def map[B](f: A => B): Task[B] = flatMap(f andThen (Task.no))

  def attempt:Task[Either[Throwable,A]] = Task(get map{
    case Left(e) => Right(Left(e))
    case Right(a) => Right(Right(a))
  })
}

object Task extends Monad[Task] {
  override def unit[A](a: => A): Task[A] = Task(IO(Try(a)))

  override def flatMap[A, B](a: Task[A])(f: A => Task[B]): Task[B] = a flatMap (f)

  def fail[A](e: Throwable): Task[A] = Task(IO(Left(e)))

  def now[A](a: A): Task[A] = Task(Return(Right(a)))

  def Try[A](a: => A): Either[Throwable, A] = try Right(a) catch {
    case e: Throwable => Left(e)
  }
}