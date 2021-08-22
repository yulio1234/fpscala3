package function.localEffects

import scala.reflect.ClassTag

object Mutale {

}

sealed trait ST[S, A] {
  self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    override protected def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    override protected def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S, A](a: => A) = {
    lazy val memo = a
    new ST[S, A] {
      override protected def run(s: S): (A, S) = (memo, s)
    }
  }

  def runST[A](st: RunableST[A]): A = st[Unit].run(())._1
}

sealed trait STRef[S, A] {
  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    override protected def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    var cell = a
  })
}

val q = for {
  r1 <- STRef[Nothing, Int](1)
  r2 <- STRef[Nothing, Int](1)
  x <- r1.read
  y <- r2.read
  _ <- r1.write(y+1)
  _ <- r2.write(x+1)
  a <- r1.read
  b <- r2.read
} yield (a,b)

trait RunableST[A] {
  def apply[S]: ST[S, A]
}

val p = new RunableST[(Int,Int)] {
  override def apply[S]: ST[S, (Int, Int)] = for {
    r1 <- STRef(1)
    r2 <- STRef(1)
    x <- r1.read
    y <- r2.read
    _ <- r1.write(y+1)
    _ <- r2.write(x+1)
    a <- r1.read
    b <- r2.read
  } yield (a,b)
}

sealed abstract class STArray[S, A](implicit manifest: ClassTag[A]) {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.size)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  /**
   * 练习14.1
   * @param xs
   * @return
   */
  def fill(xs: Map[Int, A]): ST[S, Unit] = xs.foldRight(ST[S, Unit](())) {
    case ((k, v), st) => st.flatMap(_ => write(k, v))
  }

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}

object STArray {
  def apply[S, A: ClassTag](sz: Int, v: A): ST[S, STArray[S, A]] = ST(new STArray[S, A]() {
    lazy val value = Array.fill(sz)(v)
  })

  def fromList[S, A: ClassTag](xs: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A]() {
      lazy val value = xs.toArray
    })
}

//object Immutable {
//  def noop[S] = ST[S,Unit](())
//  def partition[S](a: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] = for {
//    vp <- a.read(pivot)
//    _ <- a.swap(pivot,r)
//    j <-  STRef(l)
//    _ <- (l until r).foldLeft(noop[S])((s,i)=>for {
//      _ <- s
//      vi <- a.read(i)
//      _ <-
//    })
//  }
//}

