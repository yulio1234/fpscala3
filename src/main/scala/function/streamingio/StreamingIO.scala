package function.streamingio

import function.iomonad.IO0.fahrenheitToCelsius
import function.iomonad.{IO, Monad, unsafePerformIO}
import function.streamingio.GeneralizedLazyListTransducers.Process

import java.io.{BufferedReader, FileReader, FileWriter}
import java.util.concurrent.Executors
import language.implicitConversions
import language.higherKinds
import language.postfixOps

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

    def repeat: Process[F, O] = this ++ this.repeat

    def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
      def go(cur: Process[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] = cur match {
        case Emit(h, t) => go(t, acc :+ h)
        case Halt(End) => F.unit(acc)
        case Halt(err) => F.fail(err)
        case Await(req, recv) => F.flatMap(F.attempt(req)) { e => go(Try(recv(e)), acc) }
      }

      go(this, IndexedSeq())
    }

    def |>[O2](p2: Process1[O, O2]): Process[F, O2] = {
      p2 match {
        case Halt(e) => this.kill onHalt { e2 => Halt(e) ++ Halt(e) }
        case Emit(h, t) => Emit(h, this |> t)
        case Await(req, recv) => this match {
          case Halt(err) => Halt(err) |> recv(Left(err))
          case Emit(h, t) => t |> Try(recv.asInstanceOf[Either[Throwable, O] => Process[Is[O]#f, O2]](Right(h)))
          case Await(req0, recv0) => await(req0)(recv0 andThen (_ |> p2))
        }
      }
    }

    final def kill[O2]: Process[F, O2] = this match {
      case Await(req, recv) => recv(Left(Kill)).drain.onHalt {
        case Kill => Halt(End)
        case e => Halt(e)
      }
      case Halt(e) => Halt(e)
      case Emit(h, t) => t.kill
    }

    def filter(f: O => Boolean): Process[F, O] = this |> Process.filter(f)

    def pipe[O2](p2: Process1[O, O2]): Process[F, O2] = this |> p2

    def take(n: Int): Process[F, O] = this |> Process.take(n)

    def once: Process[F, O] = take(1)

    def asFinalizer: Process[F, O] = this match {
      case Emit(h, t) => Emit(h, t.asFinalizer)
      case Halt(e) => Halt(e)
      case Await(req, recv) => await(req) {
        case Left(Kill) => this.asFinalizer
        case x => recv(x)
      }
    }

    def onComplate(p: => Process[F, O]): Process[F, O] = this.onHalt {
      case End => p.asFinalizer
      case err => p.asFinalizer ++ Halt(err)
    }

    final def drain[O2]: Process[F, O2] = this match {
      case Halt(e) => Halt(e)
      case Emit(h, t) => t.drain
      case Await(req, recv) => Await(req, recv andThen (_.drain))
    }

    def tee[O2, O3](p2: Process[F, O2])(t: Tee[O, O2, O3]): Process[F, O3] = {
      t match {
        case Halt(e) => this.kill onComplate p2.kill onComplate Halt(e)
        case Emit(h, t) => Emit(h, (this tee p2) (t))
        case Await(side, recv) => side.get match {
          case Left(isO) => this match {
            case Halt(e) => p2.kill onComplate Halt(e)
            case Emit(o, ot) => (ot tee p2) (Try(recv.asInstanceOf[Either[Throwable, O] => Process[T[O, O2]#f, O3]](Right(o))))
            case Await(reqL, recvL) => await(reqL)(recvL andThen (this2 => this2.tee(p2)(t)))
          }
          case Right(isO2) => p2 match {
            case Halt(e) => this.kill onComplate Halt(e)
            case Emit(o2, ot) => (this tee ot) (Try(recv.asInstanceOf[Either[Throwable, O2] => Process[T[O, O2]#f, O3]](Right(o2))))
            case Await(reqR, recvR) => await(reqR)(recvR andThen (p3 => this.tee(p3)(t)))
          }
        }
      }
    }

    def zipWith[O2, O3](p2: Process[F, O2])(f: (O, O2) => O3): Process[F, O3] = (this tee p2) (Process.zipWith(f))

    def zip[O2](p2: Process[F, O2]): Process[F, (O, O2)] = zipWith(p2)((_, _))

    def to[O2](sink: Sink[F, O]): Process[F, Unit] = join {
      (this zipWith sink) ((o, f) => f(o))
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

    def resource[R, O](acquire: IO[R])(use: R => Process[IO, O])(release: R => Process[IO, O]): Process[IO, O] =
      eval(acquire) flatMap { r => use(r).onComplate(release(r)) }


    def lines(filename: String): Process[IO, String] =
      resource {
        IO(io.Source.fromFile(filename))
      } { src =>
        val iter = src.getLines()
        val step = if (iter.hasNext) Some(iter.next()) else None
        lazy val lines: Process[IO, String] = eval(IO(step)).flatMap {
          case None => Halt(End)
          case Some(line) => Emit(line, lines)
        }
        lines
      } { src => eval_(IO(src.close())) }

    def eval[F[_], A](a: F[A]): Process[F, A] = await[F, A, A](a) {
      case Left(err) => Halt(err)
      case Right(a) => Emit(a, Halt(End))
    }

    def eval_[F[_], A, B](a: F[A]): Process[F, B] = eval[F, A](a).drain[B]

    case class Is[I]() {
      sealed trait f[X]

      val Get = new f[I] {}
    }

    def Get[I] = Is[I]().Get

    type Process1[I, O] = Process[Is[I]#f, O]

    def await1[I, O](recv: I => Process1[I, O], fallback: => Process1[I, O] = halt1[I, O]) = Await(Get[I], (e: Either[Throwable, I]) => e match {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(i) => Try(recv(i))
    })

    def emit1[I, O](h: O, t1: Process1[I, O] = halt1[I, O]): Process1[I, O] = emit1(h, t1)

    def halt1[I, O]: Process1[I, O] = Halt[Is[I]#f, O](End)

    def lift[I, O](f: I => O): Process1[I, O] = await1[I, O]((i: I) => emit(f(i))) repeat

    def filter[I](f: I => Boolean): Process1[I, I] = await1(i => if (f(i)) emit(i) else halt1)

    def take[I](n: Int): Process1[I, I] = if (n <= 0) halt1 else await1[I, I](i => emit(i, take(n - 1)))

    def id[I]: Process1[I, I] = await1((i: I) => emit1(i, id))

    def intersperse[I](sep: I): Process1[I, I] = await1[I, I](i => emit1(i) ++ id.flatMap(i => emit1(sep) ++ emit1(i)))

    case class T[I, I2]() {
      sealed trait f[X] {
        def get: Either[I => X, I2 => X]
      }

      val L = new f[I] {
        def get = Left(identity)
      }
      val R = new f[I2] {
        def get = Right(identity)
      }
    }

    def L[I, I2] = T[I, I2]().L

    def R[I, I2] = T[I, I2]().R

    type Tee[I, I2, O] = Process[T[I, I2]#f, O]

    def haltT[I, I2, O]: Tee[I, I2, O] = Halt[T[I, I2]#f, O](End)

    def awaitL[I, I2, O](recv: I => Tee[I, I2, O], fallback: => Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] = await[T[I, I2]#f, I, O](L) {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(a) => Try(recv(a))
    }

    def awaitR[I, I2, O](recv: I2 => Tee[I, I2, O], fallback: => Tee[I, I2, O] = haltT[I, I2, O]) = await[T[I, I2]#f, I2, O](R) {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(a) => Try(recv(a))
    }

    def emitT[I, I2, O](h: O, tl: Tee[I, I2, O] = haltT[I, I2, O]) = emit(h, tl)

    def zipWith[I, I2, O](f: (I, I2) => O): Tee[I, I2, O] = awaitL[I, I2, O](i => awaitR((i2 => emitT(f(i, i2))))) repeat

    def zip[I, I2]: Tee[I, I2, (I, I2)] = zipWith((_, _))

    type Sink[F[_], O] = Process[F, O => Process[F, Unit]]

    def fileW(file: String, append: Boolean = false): Sink[IO, String] = resource[FileWriter, String => Process[IO, Unit]] {
      IO {
        new FileWriter(file, append)
      }
    } { w => constant { (s: String) => eval[IO, Unit](IO(w.write(s))) } } { w =>
      eval_(IO {
        w.close
      })
    }


    def constant[A](a: A): Process[IO, A] = eval(IO(a)).flatMap { a => Emit(a, constant(a)) }

    def join[F[_], O](p: Process[F, Process[F, O]]): Process[F, O] = p.flatMap(pa => pa)

    val converter: Process[IO, Unit] = lines("f.txt")
      .filter(!_.startsWith("#"))
      .map(line => fahrenheitToCelsius(line.toDouble).toString)
      .pipe(intersperse("\n"))
      .to(fileW("celsius.text"))
      .drain

    type Channel[F[_], I, O] = Process[F, I => Process[F, O]]

    val converALl: Process[IO, Unit] = (for {
      out <- fileW("celsius.text").once
      file <- lines("f.text")
      _ <- lines(file).map(line => fahrenheitToCelsius(line.toDouble)).flatMap(celsius => out(celsius.toString))
    } yield ()) drain
  }
}
