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
    case Branch(l, r) => 1 + depth(l) max depth(r)
  }

  // Exercise 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 3.29 ... implement generilized fold function and define
  // maximum, depth and size using that.
  def fold[A,B](t: Tree[A], z: B) = ???

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

    println(sTree)
    println(size(sTree))
    println(maximum(iTree))
    println(depth(iTree))
    println(mapped)
  }
}