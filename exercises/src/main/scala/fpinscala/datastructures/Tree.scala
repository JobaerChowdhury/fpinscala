package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  // Exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // Exercise 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 3.29 ... implement generilized fold function and define
  // maximum, depth and size using that.
  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(v) => l(v)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }

  def size2[A](t: Tree[A]): Int = fold(t)(a => 1)((b1, b2) => 1 + b1 + b2)

  def maximum2(t: Tree[Int]): Int = fold(t)(a => a)((b1,b2) => b1 max b2)

  def depth2[A](t: Tree[A]): Int = fold(t)(a => 1)((b1, b2) => 1 + (b1 max b2))

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(l => Leaf(f(l)) : Tree[B])((b1, b2) => Branch(b1, b2))

}

object TreeTest {
  import Tree._

  def main (args: Array[String]) {
    println("Testing tree ....")

    val aL = Leaf("a")
    val bL = Leaf("b")
    val cL = Leaf("c")
    val dL = Leaf("d")
    val sTree = Branch(Branch(aL, bL), Branch(cL, dL))
    val iTree = Branch(Branch(Leaf(11), Leaf(9)), Branch(Branch(Leaf(5), Leaf(7)), Leaf(3)))

    val mapped = map(iTree) (a => 1)
    val mapped2 = map(iTree) (a => 7)

    println(sTree)

    println(size2(sTree))
    println(size(sTree))

    println(maximum2(iTree))
    println(maximum(iTree))

    println(depth2(iTree))
    println(depth(iTree))

    println(mapped)
    println(mapped2)
  }
}