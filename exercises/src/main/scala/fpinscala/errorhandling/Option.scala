package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] =
      this map(Some(_)) getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
      flatMap(a => if(f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs map (e => math.pow(e-m,2))))
  }

  // Exercise 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(x => b map (y => f(x,y)))

  // Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x :: xs => x flatMap (xx => sequence(xs) map (xx :: _))
  }

  // Implement using foldRight
  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil))((opa,b) => ???)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sys.error("todo")
}

object OptionTest{
  import Option._
  def main (args: Array[String]) {
    println("Testing Options")

    println(variance(Seq()))

    println(map2(Some(1), Some(2))(_ + _))
    println(map2(None:Option[Int], Some(2))(_ + _))

    val resSeq1 = sequence(List(Some(1), Some(2), Some(3)))
    println(resSeq1)
    println(sequence2(List(Some(1), Some(2), Some(3))))

    val resSeq2 = sequence(List(Some(1), None, Some(3)))
    println(resSeq2)
  }
}