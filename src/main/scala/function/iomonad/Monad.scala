package function.iomonad

trait Functor[F[_]] {
  def map[A, B](a: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](a: F[A])(f: A => F[B]): F[B]

  def map[A, B](a: F[A])(f: A => B): F[B] = flatMap(a)(a => unit(f(a)))

  def map2[A, B, C](a: F[A], b: F[B])(f: (A, B) => C): F[C] = flatMap(a)(a => map(b)(b => f(a, b)))

  /**
   * 循环执行n次，并获取返回
   *
   * @param n
   * @param f
   * @tparam A
   * @return
   */
  def replicateM[A](n: Int)(f: F[A]): F[List[A]] = LazyList.fill(n)(f).foldRight(unit(List[A]()))(map2(_, _)(_ :: _))

  /**
   * 替换内部参数
   *
   * @param a
   * @param b
   * @tparam A
   * @tparam B
   * @return
   */
  def as[A, B](a: F[A])(b: B): F[B] = map(a)(_ => b)

  /**
   * 跳过执行
   *
   * @param a
   * @tparam A
   * @return
   */
  def skip[A](a: F[A]): F[Unit] = as(a)(())

  /**
   * 折叠单子
   *
   * @param l
   * @param z
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def foldM[A, B](l: LazyList[A])(z: B)(f: (B, A) => F[B]): F[B] = l match {
    case h #:: t => f(z, h).flatMap(z2 => foldM(t)(z2)(f))
    case _ => unit(z)
  }

  def foldM_[A, B](lazyList: LazyList[A])(z: B)(f: (B, A) => F[B]): F[Unit] = skip {
    foldM(lazyList)(z)(f)
  }

  def foreachM[A](l: LazyList[A])(f: A => F[Unit]): F[Unit] = foldM_(l)(())((u, a) => skip(f(a)))

  implicit def toMonadic[A](a: F[A]): Monadic[F, A] = new Monadic[F, A] {
    val F = Monad.this

    def get = a
  }
}

trait Monadic[F[_], A] {
  val F: Monad[F]

  def get: F[A]

  private val a = get

  def map[B](f: A => B): F[B] = F.map(a)(f)

  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(a)(f)

  def **[B](b: F[B]) = F.map2(a, b)((_, _))
  
  def skip:F[Unit] = F.skip(a)
}