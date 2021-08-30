package function.streamingio

import function.iomonad.{IO, Monad, unsafePerformIO}
import function.streamingio.GeneralizedLazyListTransducers.Process

import java.io.{BufferedReader, FileReader}
import java.util.concurrent.Executors

object ImperativeAndLazyIO {

  import java.io._


  def lines(filename: String): IO[LazyList[String]] = IO {
    val src = io.Source.fromFile(filename)
    src.getLines().to(LazyList) ++ {
      src.close();
      LazyList.empty
    }
  }
}

object SimpleLazyListTransducers {
  sealed trait Process[I, O] {

    import Process._

    def apply(s: LazyList[I]): LazyList[O] = this match {
      case Halt() => LazyList()
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs => recv(None)(xs) //lazyList是空的
      }
      case Emit(head, tail) => head #:: tail(s)
    }

    def map[O2](f: O => O2): Process[I, O2] = this match {
      case Halt() => Halt()
      case Emit(h, t) => Emit(f(h), t map (f))
      case Await(recv) => Await(recv andThen (_ map (f)))
    }

    def ++(p: => Process[I, O]): Process[I, O] = this match {
      case Halt() => p
      case Emit(h, t) => Emit(h, t ++ p)
      case Await(recv) => Await(recv andThen (_ ++ p))
    }

    def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
      case Halt() => Halt()
      case Emit(h, t) => f(h) ++ t.flatMap(f)
      case Await(recv) => Await(recv andThen (_ flatMap (f)))
    }

    def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] = p match {
        case Halt() => go(this) //如果过程是停止就重启自身
        case Await(recv) => Await {
          case None => recv(None)
          case i => go(recv(i))
        }
        case Emit(h, t) => Emit(h, go(t))
      }

      go(this)
    }

    def |>[O2](p2: Process[O, O2]): Process[I, O2] = {
      p2 match {
        case Halt() => Halt()
        case Emit(h, t) => Emit(h, this |> t)
        case Await(f) => this match {
          case Emit(h, t) => t |> f(Some(h))
          case Halt() => Halt() |> f(None)
          case Await(g) => Await((i: Option[I]) => g(i) |> p2)
        }
      }
    }

    def filter(f: O => Boolean): Process[I, O] = this |> Process.filter(f)

    def zip[O2](p: Process[I, O2]): Process[I, (O, O2)] = Process.zip(this, p)

    def zipWithIndex: Process[I, (O, Int)] = this.zip(count map (_ - 1))

  }

  object Process {
    case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]

    case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]

    case class Halt[I, O]() extends Process[I, O]

    def emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()): Process[I, O] = Emit(head, tail)

    def monad[I]: Monad[({type f[x] = Process[I, x]})#f] = new Monad[({type f[x] = Process[I, x]})#f] {
      override def unit[A](a: => A): Process[I, A] = emit(a)

      override def flatMap[A, B](a: Process[I, A])(f: A => Process[I, B]): Process[I, B] = a flatMap f
    }

    def await[I, O](f: I => Process[I, O], fallback: Process[I, O] = Halt[I, O]()): Process[I, O] = Await[I, O] {
      case Some(i) => f(i)
      case None => fallback
    }

    def liftOne[I, O](f: I => O): Process[I, O] = Await {
      case Some(i) => Emit(f(i))
      case None => Halt()
    }

    def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

    def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
      case Some(i) if p(i) => emit(i)
      case _ => Halt()
    }.repeat

    def sum: Process[Double, Double] = {
      def go(acc: Double): Process[Double, Double] = Await {
        case Some(d) => Emit(d + acc, go(d + acc))
        case None => Halt()
      }

      go(0.0)
    }

    def take[I](n: Int): Process[I, I] = if (n <= 0) Halt() else await(i => emit(i, take[I](n - 1)))

    def drop[I](n: Int): Process[I, I] = if (n <= 0) id else await(i => drop[I](n - 1))

    def takeWhile[I](f: I => Boolean): Process[I, I] = await(i =>
      if (f(i)) emit(i, takeWhile(f))
      else Halt()
    )

    def dropWhile[I](f: I => Boolean): Process[I, I] = await(i =>
      if (f(i)) dropWhile(f)
      else emit(i, id)
    )

    def id[I]: Process[I, I] = lift(identity)

    def count[I]: Process[I, Int] = {
      def go(n: Int): Process[I, Int] = await((i: I) => emit(n + 1, go(n + 1)))

      go(0)
    }

    def mean: Process[Double, Double] = {
      def go(sum: Double, count: Double): Process[Double, Double] = {
        await((d: Double) => emit((sum + d) / (count + 1), go(sum + d, count + 1)))
      }

      go(0.0, 0.0)
    }

    def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] = await((i: I) => f(i, z) match {
      case (o, s2) => emit(o, loop(s2)(f))
    })

    /**
     * 练习16.4
     *
     * @return
     */
    def sum2: Process[Double, Double] = loop(0.0)((d: Double, acc) => (acc + d, acc + d))

    def count2[I]: Process[I, Int] = loop(0)((_: I, n) => (n + 1, n + 1))

    def zip[A, B, C](p1: Process[A, B], p2: Process[A, C]): Process[A, (B, C)] = (p1, p2) match {
      case (Halt(), _) => Halt()
      case (_, Halt()) => Halt()
      case (Emit(b, t1), Emit(c, t2)) => Emit((b, c), zip(t1, t2))
      case (Await(recv1), _) => Await((oa: Option[A]) => zip(recv1(oa), feed(oa)(p2)))
      case (_, Await(recv2)) => Await((oa: Option[A]) => zip(feed(oa)(p1), recv2(oa)))
    }

    def feed[A, B](oa: Option[A])(p: Process[A, B]): Process[A, B] = p match {
      case Halt() => p
      case Emit(h, t) => Emit(h, feed(oa)(t))
      case Await(recv) => recv(oa)
    }

    val mean2 = (sum zip count) |> lift { case (s, n) => s / n }

    def processFile[A, B](f: java.io.File, p: Process[String, A], z: B)(g: (B, A) => B): IO[B] = IO {
      def go(ss: Iterator[String], cur: Process[String, A], acc: B): B = cur match {
        case Halt() => acc
        case Await(recv) =>
          val next = if (ss.hasNext) recv(Some(ss.next())) else recv(None)
          go(ss, next, acc)
        case Emit(h, t) => go(ss, t, g(acc, h))
      }

      val s = io.Source.fromFile(f)
      try go(s.getLines(), p, z) finally s.close()
    }

    def toCelsius(fahrenheit: Double): Double = (5.0 / 9.0) * (fahrenheit - 32.0)

    def convertFahrenheit: Process[String, String] =
      filter((line: String) => !line.startsWith("#")) |>
        filter(line => line.trim.nonEmpty) |>
        lift(line => toCelsius(line.toDouble).toString)
  }

}

object GeneralizedLazyListTransducers {

  import Process._

  trait Process[F[_], O] {
    def map[O2](f: O => O2): Process[F, O2] = this match {
      case Await(req, recv) => Await(req, recv andThen (_ map (f)))
      case Emit(h, t) => Try {
        Emit(f(h), t map f)
      }
      case Halt(err) => Halt(err)
    }

    def ++(p: => Process[F, O]): Process[F, O] = this.onHalt {
      case End => Try(p)
      case err => Halt(err)
    }

    def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
      case Halt(e) => Try(f(e))
      case Emit(h, t) => Emit(h, t.onHalt(f))
      case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
    }

    def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = this match {
      case Halt(err) => Halt(err)
      case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
      case Await(req, recv) => Await(req, recv andThen (_ flatMap (f)))
    }

    def runLog(implicit F:MonadCatch[F]):F[IndexedSeq[O]] = {
      def go(cur:Process[F,O],acc:IndexedSeq[O]):F[IndexedSeq[O]] = cur match {
        case Emit(h,t) => go(t,acc :+ h)
        case Halt(End) => F.unit(acc)
        case Halt(err) => F.fail(err)
        case Await(req,recv) => F.flatMap(F.attempt(req)) {e=>go(Try(recv(e)),acc)}
      }
      go(this,IndexedSeq())
    }
  }

  object Process {
    case class Await[F[_], A, O](req: F[A], recv: Either[Throwable, A] => Process[F, O]) extends Process[F, O]

    case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]

    case class Halt[F[_], O](err: Throwable) extends Process[F, O]

    case object End extends Exception

    case object Kill extends Exception

    def emit[F[_], O](head: O, tail: Process[F, O] = Halt[F, O](End)): Process[F, O] = Emit(head, tail)

    def await[F[_], A, O](req: F[A])(recv: Either[Throwable, A] => Process[F, O]) = Await(req, recv)

    def Try[F[_], O](p: => Process[F, O]): Process[F, O] = try p catch {
      case e: Throwable => Halt(e)
    }

    def runLog[O](src: Process[IO, O]): IO[IndexedSeq[O]] = IO {
      val E = Executors.newFixedThreadPool(4)

      def go(cur: Process[IO, O], acc: IndexedSeq[O]): IndexedSeq[O] = cur match {
        case Emit(h, t) => go(t, acc :+ h)
        case Halt(End) => acc
        case Halt(err) => throw err
        case Await(req, recv) =>
          val next = try recv(Right(unsafePerformIO(req)(E)))
          catch {
            case err: Throwable => recv(Left(err))
          }
          go(next, acc)
      }

      try go(src, IndexedSeq())
      finally E.shutdown()
    }

    val p: Process[IO, String] = await(IO(new BufferedReader(new FileReader("line.text")))) {
      case Right(b) =>
        lazy val next: Process[IO, String] = await(IO(b.readLine())) {
          case Left(e) => await(IO(b.close()))(_ => Halt(e))
          case Right(line) => if (line eq null) Halt(End) else Emit(line, next)
        }
        next
      case Left(e) => Halt(e)
    }
  }
}
