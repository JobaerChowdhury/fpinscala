package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def headOption2: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // Exercise 5.6 ... headOption using foldRight
  def headOption: Option[A] =
    foldRight(None: Option[A])((a,b) => Some(a))

  // Exercise 5.1
  def toList2: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // Exercise 5.7 ... implement map, filter, append and flatMap
  def map[B](f: A => B): Stream[B] =
      foldRight(empty: Stream[B])((a,b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
      foldRight(empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)

  // todo -- why this type signature... what does it mean .. find out.
  def append[B >: A] (b: Stream[B]): Stream[B] =
      foldRight(b: Stream[B])((x, y) => cons(x, y))

  def flatMap[B](f : A => Stream[B]): Stream[B] =
      foldRight(empty:Stream[B])((a,b) => f(a) append b )

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = {
      s match {
        case Cons(h, t) => go(t(), h()::acc)
        case _ => acc
      }
    }
    go(this, List()).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  // Exercise 5.5 ... implement takeWhile using foldRight
  def takeWhile2(p: A => Boolean): Stream[A] =
     foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}


object StreamTest {
  import Stream._

  def main(args: Array[String]) {
    val test: Stream[Int] = Stream.apply(1,2,3,4,5)

    println(ones.headOption)
    println(test.toList)

    println(test.drop(3).toList)
    println(test.take(3).toList)

    println(test.map(_ + 3).toList)

    println(test.filter(_ > 3).toList)
    println(test.append(test).toList)

    println(test.takeWhile(_ < 5).toList)

    println(test.forAll( _ > 1))

    println("Testing streams ....")
  }
}
