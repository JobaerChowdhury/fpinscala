package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def sum3(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x+y)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("empty list")
      case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(x, xs) => Cons(h, xs)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n < 0) sys.error("Negative index")
    else l match {
      case Nil => sys.error("Empty list")
      case Cons(x, xs) =>
        if (n == 0) l
        else drop(xs, n-1)
    }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def reverse[A](l: List[A]): List[A] = {
    def go(l: List[A], acc: List[A]): List[A] = l match {
      case Nil => acc
      case Cons(x, xs) => go(xs, Cons(x, acc))
    }

    go(l, Nil)
  }

  def reverse2[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((xs, x) => Cons(x, xs))

  def init[A](l: List[A]): List[A] = reverse(tail(reverse(l)))

  def length[A](l: List[A]): Int = foldRight(l, 0)((a, b) => b+1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs,f(z,x))(f)
  }

  // Exercise 3.13 .. implement using foldRight
  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, z)((a: A, b: B) => f(b,a))

  // implement using foldLeft
  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b: B, a:A) => f(a,b))

  // Exercise 3.14 .. implement using foldLeft/foldRight
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((xs, x) => Cons(xs,x))

  //Exercise 3.15 ... concatenates a list of lists into a single list
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  //Exercise 3.16
  def addOne(l: List[Int]): List[Int] = map(l)(x => x + 1)

  //Exercise 3.17
  def mkString(l: List[Double]): List[String] =
      map(l)(x => x.toString)

  // Exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) Cons(x, filter(xs)(f))
      else filter(xs)(f)
  }

  //Exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  //Exercise 3.21 ... implement filter using flatMap
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)(a => if(f(a)) List(a) else Nil)

  // Exercise 3.22 ... Write a function that accepts two lists and constructs a new list by adding correspond- ing elements.
  // For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
  def addTwo(fst: List[Int], snd: List[Int]): List[Int] = fst match {
    case Nil => Nil
    case Cons(x, xs) => snd match {
      case Nil => Nil
      case Cons (y, ys) => Cons(x+y, addTwo(xs, ys))
    }
  }

  // Exercise 3.23 ... implement zipWith
  def zipWith[A, B, C](fst: List[A], snd: List[B])(f: (A, B) => C): List[C] = fst match {
    case Nil => Nil
    case Cons(x, xs) => snd match {
      case Nil => Nil
      case Cons(y, ys) => Cons(f(x, y), zipWith(xs, ys)(f))
    }
  }

  // Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
}

object ListTest {
  def main(args: Array[String]) = {
    import List._
    println("Testing list data structure")

    val testList = Cons(1, Cons(2, Cons(4, Nil)))
    val list2 = Cons(5, Cons(6, Cons(7, Nil)))
    val dList = Cons(1.0, Cons(3.9, Cons(4.1, Nil)))

    val ll = Cons(testList, Cons(list2, Nil))

    println(tail(testList))
//    println(tail(Nil))
    println(setHead(testList, 5))
    println(drop(testList, 1))

    println(dropWhile(testList, (x:Int) => x < 3))

    println(reverse(testList))
    println(init(testList))

    println(length(testList))
    println(sum3(testList))

    println(reverse2(testList))
    println(append2(list2, testList))

    println(ll)
    println(concat(ll))

    val r1 = foldLeft2(testList, 10)(_ + _)
    println(r1)

    val r2 = foldRight2(testList, 11)(_ + _)
    println(r2)

    println(addOne(testList))
    println(append(Cons("abcd", Nil), mkString(dList)))

    println(filter(testList)(x => x < 3))
    println(filter2(testList)(x => x < 3))

    println(addTwo(testList, testList))

    println(zipWith(testList, testList)(_ * _))
  }
}