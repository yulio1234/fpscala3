package function.iomonad

import scala.io.StdIn.readLine

/**
 * 只支持输出
 */
object IO0 {
  trait IO {
    self =>
    def run: Unit

    def ++(io: IO): IO = new IO {
      override def run: Unit = {
        self.run
        io.run
      }
    }
  }

  object IO {
    def empty: IO = new IO {
      override def run: Unit = ()
    }
  }

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

}

/**
 * 支持输入输出类型
 */
object IO1 {
  trait IO[A] {
    self =>
    def run: A

    def map[B](f: A => B): IO[B] = new IO[B] {
      override def run: B = f(self.run)
    }

    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
      override def run: B = f(self.run).run
    }
  }

  object IO extends Monad[IO] {
    override def unit[A](a: => A): IO[A] = new IO[A] {
      override def run: A = a
    }

    override def flatMap[A, B](a: IO[A])(f: A => IO[B]): IO[B] = a.flatMap(f)

    def apply[A](a: => A): IO[A] = unit(a)

    def ref[A](a: A): IO[IORef[A]] = IO {
      new IORef(a)
    }

    /**
     * 可变引用
     *
     * @param value
     * @tparam A
     */
    sealed class IORef[A](var value: A) {
      def set(a: A): IO[A] = IO {
        value = a
        a
      }

      def get: IO[A] = IO {
        value
      }

      def modify(f: A => A): IO[A] = get.flatMap(a => set(f(a)))
    }
  }

  def ReadLine: IO[String] = IO {
    readLine()
  }

  def PrintLine(msg: String): IO[Unit] = IO {
    println(msg)
  }

  import IO0.fahrenheitToCelsius

  def converter: IO[Unit] = for {
    _ <- PrintLine("输入一个温度")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  import IO._

  //从命令行读取并打印
  val echo = ReadLine.flatMap(PrintLine)
  //读取一行转换为int
  val readInt = ReadLine.map(_.toInt)
  //读取两行解析并返回(Int,Int)
  val readInts = readInt ** readInt
  //读取10行，并返回一个list
  private val lines: IO[List[String]] = replicateM(10)(ReadLine)

  def factorial(n: Int): IO[Int] = for {
    acc <- ref(1)
    _ <- foreachM(1 to n to(LazyList))(i => acc.modify(_ * i).skip)
    result <- acc.get
  } yield result
}
