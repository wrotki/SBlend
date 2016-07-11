package playground

import scalaz._
import Scalaz._
import scalaz.Monoid

/**
  * Created by mariusz on 7/9/16.
  */
object MethInj {


  def plus[A: Monoid](a: A, b: A): A = {
    val mnd: Monoid[A] = implicitly[Monoid[A]]
    mnd.append(a, b)
  }

}
