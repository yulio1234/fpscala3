package function.streamingio

import function.streamingio.SimpleLazyListTransducers.Process.Emit

object StreamingIOTest extends App {

  import SimpleLazyListTransducers.Process.liftOne

  val p = liftOne((x: Int) => x * 2)
  private val value = p(LazyList(1, 2, 3))
  private val list: List[Int] = value.toList
  println(list)

}
