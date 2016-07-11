package playground

import scalaz._
import Scalaz._

/**
  * Created by mariusz on 7/10/16.
  */
object ListOptionTExample {



  case class Record(fields: Option[Seq[Int]])

  type ListTOption[A, B] = Kleisli[Option, A, B]
  object ListTOption {
    def ro[A, B](f: A => Option[B]): ListTOption[A, B] = Kleisli(f)
  }

  def runex: Unit = {

  }
}

object UnderscoreEx {
  def runex: Unit = {

    type Error[+A] = \/[String, A]
    type Result[A] = OptionT[Error, A]

    val result: Result[Int] = 42.point[Result]
    val transformed: OptionT[Error, String] =
      for {
        value <- result
      } yield value.toString
    val ins = transformed.run.fold(
      l = err => "So broken",
      r = _.get
    )
    println(ins)
  }
}
