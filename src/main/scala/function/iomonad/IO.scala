package function.iomonad


import function.iomonad.IO2a.IO
import function.iomonad.IO2c.FlatMap
import function.iomonad.IO3.Console.ConsoleIO
import function.parallelism.Nonblocking.Par

import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import scala.annotation.tailrec
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

  val helpstring =
    """
      | The Amazing Factorial REPL, v2.0
      | q - quit
      | <number> - compute the factorial of the given number
      | <anything else> - bomb with horrible error
  """.trim.stripMargin

  def factorial(n: Int): IO[Int] = for {
    acc <- ref(1)
    _ <- foreachM(1 to n to (LazyList))(i => acc.modify(_ * i).skip)
    result <- acc.get
  } yield result

  val factorialREPL: IO[Unit] = sequence_(
    IO {
      println(helpstring)
    },
    doWhile {
      IO {
        readLine()
      }
    } { line =>
      when(line != "q") {
        for {
          n <- factorial(line.toInt)
          _ <- IO {
            println("factorial:" + n)
          }
        } yield ()
      }
    }
  )
}

/**
 * 不会栈溢出的io类型
 */
object IO2a {
  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

    def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
  }

  //没有其他步骤，立即返回A
  case class Return[A](a: A) extends IO[A]

  //计算暂停，不接收任何参数并作用产生结果
  case class Suspend[A](resume: () => A) extends IO[A]

  //两个步骤的组合，flatmap转换为数据类型而不是函数
  case class FlatMap[A, B](sub: IO[A], f: A => IO[B]) extends IO[B]

  def printLine(s: String): IO[Unit] = Suspend(() => Return(println(s)))

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = Return(a)

    override def flatMap[A, B](a: IO[A])(f: A => IO[B]): IO[B] = a flatMap f

    //什么都不做，去实例化a
    def suspend[A](a: => IO[A]) = Suspend(() => ()).flatMap(_ => a)
  }

  val p = IO.forever(printLine("Still going..."))

  @tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match { //可以直接run(f(run(x))),但run不是在尾部
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
}

object IO2aTest {

  import IO2a._

  val f: Int => IO[Int] = (i: Int) => Return(i)
  val g = List.fill(100000)(f).foldLeft(f) {
    (a, b) => x => Suspend(() => ()).flatMap(_ => a(x).flatMap(b))
  }

  def main(args: Array[String]): Unit = {
    val value = g(42)
    println(run(value))

  }
}

/**
 * 尾递归消除
 */
object IO2b {
  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)

    def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))
  }

  //没有其他步骤，立即返回A
  case class Return[A](a: A) extends TailRec[A]

  //计算暂停，不接收任何参数并作用产生结果
  case class Suspend[A](resume: () => A) extends TailRec[A]

  //两个步骤的组合，flatmap转换为数据类型而不是函数
  case class FlatMap[A, B](sub: TailRec[A], f: A => TailRec[B]) extends TailRec[B]
}

/**
 * 使用异步
 */
object IO2c {

  import function.parallelism.Nonblocking._

  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]): Async[B] = FlatMap(this, f)

    def map[B](f: A => B) = flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends Async[A]

  case class Suspend[A](resume: Par[A]) extends Async[A]

  case class FlatMap[A, B](sub: Async[A], f: A => Async[B]) extends Async[B]

  /**
   * 使用这个函数来单独关联flatmap
   *
   * @param async
   * @tparam A
   * @return
   */
  @tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap (g)))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a) => Par.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f) => x match {
      case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
      case _ => sys.error("不可能出现这种情况")
    }
  }
}

/**
 * 进一步抽象类型构造子
 */
object IO3 {
  /**
   * 练习13.1，实现flatmap，map，monad
   *
   * @tparam F
   * @tparam A
   */
  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)

    def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))
  }

  case class Return[F[_], A](a: A) extends Free[F, A]

  case class Suspend[F[_], A](s: F[A]) extends Free[F, A] //可以暂停任意的f

  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)

    override def flatMap[A, B](a: Free[F, A])(f: A => Free[F, B]): Free[F, B] = a flatMap (f)
  }

  /**
   * 练习13.2
   *
   * @param a
   * @tparam A
   * @return
   */
  @tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(a0, g) => runTrampoline(a0 flatMap (a0 => g(a0) flatMap (f)))
    }
  }

  @tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap (g)))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => a
  }

  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a) => F.unit(a)
    case Suspend(r) => r
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
  }

  /**
   * 限制为与控制台交互的类型
   *
   * @tparam A
   */
  sealed trait Console[A] {
    def toPar: Par[A] //将Console解释为par

    def toThunk: () => A //将Console解释为Function0[A]

    def toReader: ConsoleReader[A]

    def toState: ConsoleState[A]

  }

  case object ReadLine extends Console[Option[String]] {
    override def toPar: Par[Option[String]] = Par.lazyUnit(run)

    override def toThunk: () => Option[String] = () => run

    /**
     * 帮助函数，用于ReadLine的解释器
     *
     * @return
     */
    def run: Option[String] = try Some(readLine()) catch {
      case e: Exception => None
    }

    override def toReader: ConsoleReader[Option[String]] = ConsoleReader { in => Some(in) }

    override def toState: ConsoleState[Option[String]] = ConsoleState { bufs =>
      bufs.in match {
        case List() => (None, bufs)
        case h :: t => (Some(h), bufs.copy(in = t))
      }
    }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    override def toPar: Par[Unit] = Par.lazyUnit(println(line))

    override def toThunk: () => Unit = () => println(line)

    override def toReader: ConsoleReader[Unit] = ConsoleReader { s => () }

    override def toState: ConsoleState[Unit] = ConsoleState { bufs => ((), bufs.copy(out = bufs.out :+ line)) }
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

    def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
  }

  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G]

  implicit val function0Monad: Monad[Function0] = new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a

    override def flatMap[A, B](a: () => A)(f: A => () => B): () => B = () => f(a())()
  }
  implicit val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)

    def flatMap[A, B](a: Par[A])(f: A => Par[B]) = Par.fork {
      Par.flatMap(a)(f)
    }
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] = step(free) match {
    case Return(a) => G.unit(a)
    case Suspend(r) => t(r)
    case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
    case _ => sys.error("不可能出现这种情况")
  }

  val consoleToFunction0 = new (Console ~> Function0) {
    override def apply[A](f: Console[A]): () => A = f.toThunk
  }

  val consoleToPar = new (Console ~> Par) {
    override def apply[A](f: Console[A]): Par[A] = f.toPar
  }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A = runFree[Console, Function0, A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console, A]): Par[A] = runFree[Console, Par, A](a)(consoleToPar)

  /**
   * 练习13.4
   *
   * @param f
   * @param fg
   * @tparam F
   * @tparam G
   * @tparam A
   * @return
   */
  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[A] = Free[G, A]
    val t = new (F ~> FreeG) {
      def apply[A](a: F[A]): Free[G, A] = Suspend {
        fg(a)
      }
    }
    runFree(f)(t)(freeMonad[G])
  }

  def runConsule[A](a: Free[Console, A]): A = runTrampoline {
    translate(a)(new (Console ~> Function0) {
      override def apply[A](f: Console[A]): () => A = f.toThunk
    })
  }

  /**
   * 封装一对缓存
   *
   * @param in  缓存用于ReadLine请求的输入
   * @param out 缓存讲授PrintLine请求中的字符串
   */
  case class Buffers(in: List[String], out: Vector[String])

  case class ConsoleState[A](run: Buffers => (A, Buffers)) {
    def map[B](f: A => B): ConsoleState[B] = ConsoleState { s =>
      val (a, si) = run(s)
      (f(a), si)
    }

    def flatMap[B](f: A => ConsoleState[B]): ConsoleState[B] = ConsoleState { s =>
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  }

  object ConsoleState {
    implicit val monad: Monad[ConsoleState] = new Monad[ConsoleState] {
      override def unit[A](a: => A): ConsoleState[A] = ConsoleState(bufs => (a, bufs))

      override def flatMap[A, B](a: ConsoleState[A])(f: A => ConsoleState[B]): ConsoleState[B] = a flatMap (f)
    }
  }

  case class ConsoleReader[A](run: String => A) {
    def map[B](f: A => B): ConsoleReader[B] = ConsoleReader(r => f(run(r)))

    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] = ConsoleReader(r => f(run(r)).run(r))
  }

  object ConsoleReader {
    implicit val monad: Monad[ConsoleReader] = new Monad[ConsoleReader] {
      override def unit[A](a: => A): ConsoleReader[A] = ConsoleReader(_ => a)

      override def flatMap[A, B](a: ConsoleReader[A])(f: A => ConsoleReader[B]): ConsoleReader[B] = a flatMap (f)
    }
  }

  val consoleToState = new (Console ~> ConsoleState) {
    def apply[A](a: Console[A]) = a.toState
  }

  val consoleToReader = new (Console ~> ConsoleReader) {
    def apply[A](a: Console[A]) = a.toReader
  }

  def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] = runFree[Console, ConsoleReader, A](io)(consoleToReader)

  def runConsoleState[A](io: ConsoleIO[A]): ConsoleState[A] = runFree[Console, ConsoleState, A](io)(consoleToState)

  type IO[A] = Free[Par, A]

  def Async[A](cb: (A => Unit) => Unit): IO[A] = Suspend(Par.async(cb))

  def IO[A](a: => A): IO[A] = Suspend {
    Par.delay(a)
  }

  /**
   * 练习13.5
   * @param file
   * @param fromPosition
   * @param numBytes
   * @return
   */
  def read(file: AsynchronousFileChannel, fromPosition: Long, numBytes: Int): Par[Either[Throwable, Array[Byte]]] =
    Par.async { (cb: Either[Throwable, Array[Byte]] => Unit) =>
      val buf = ByteBuffer.allocate(numBytes)
      file.read(buf, fromPosition, (), new CompletionHandler[Integer, Unit] {
        override def completed(bytesRead: Integer, a: Unit): Unit = {
          val bytes = new Array[Byte](bytesRead)
          buf.slice().get(bytes, 0, bytesRead)
          cb(Right(bytes))
        }

        override def failed(throwable: Throwable, a: Unit): Unit = cb(Left(throwable))
      })
    }
}
