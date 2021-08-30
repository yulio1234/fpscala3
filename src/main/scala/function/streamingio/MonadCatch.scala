package function.streamingio

import function.iomonad.Task
import function.iomonad._
/**
 * 描述：
 *
 * @author yuli
 * @date 2021/8/24
 */
trait MonadCatch[F[_]] extends Monad[F] {
  def attempt[A](a: F[A]): F[Either[Throwable, A]]

  def fail[A](t: Throwable): F[A]
}

object MonadCatch {
  implicit def task: MonadCatch[Task] = new MonadCatch[Task] {

    override def unit[A](a: => A): Task[A] = Task.now(a)

    override def flatMap[A, B](a: Task[A])(f: A => Task[B]): Task[B] = a.flatMap(f)

    override def attempt[A](a: Task[A]): Task[Either[Throwable, A]] = a.attempt

    override def fail[A](t: Throwable): Task[A] = Task.fail(t)
  }
}
