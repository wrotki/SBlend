package playground

/**
  * Created by mariusz on 7/4/16.
  */
object Playground {
  def go: Unit =
  {
    val oc = Some("foo")
    val res = (Seq(1, 2, 3)).flatMap { case i =>
      (Seq("a", "b", "c")).map { case s => (i, s) }
    }
//    val res = for {
//      c <- oc
//      i <- Seq(c)
//      s <- Seq("a", "b", "c")
//    } yield (c, i, s)
    println(res)
  }
}
