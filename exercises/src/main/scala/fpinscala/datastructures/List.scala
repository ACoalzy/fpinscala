package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) if (n > 0) => drop(t, n - 1)
    case _ => l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil => Nil
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, b) => b + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def leftSum(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def leftProduct(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def leftLength[A](l: List[A]): Int =
    foldLeft(l, 0)((b, _) => b + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((t, h) => Cons(h, t))

  def foldLeftWithRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def foldRightWithLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((a, b) => f(b, a))

  def appendWithFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def flatten[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(appendWithFoldRight)
  }

  def incrementList(ints: List[Int]): List[Int] = foldRight(ints, List[Int]())((b, a) => Cons(b + 1, a))

  def convertToStringList[A](l: List[A]): List[String] = foldRight(l, List[String]())((b, a) => Cons(b.toString, a))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((h, t) => Cons(f(h), t))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, List[A]())((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = flatten(map(l)(f))

  def filterFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(h => if (f(h)) List(h) else Nil)

  def mergeIntLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (l, Nil) => l
    case (Nil, l) => l
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, mergeIntLists(t1, t2))
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => startsWith(t1, t2)
    case _ => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }

  def main(args: Array[String]): Unit = {
    println(init(List(1, 2, 3, 4)))
    println(length(List(1, 2, 3, 4, 5, 6)))
    println(leftSum(List(1, 2, 3, 4, 5)))
    println(leftProduct(List(1, 2, 3, 4, 5)))
    println(leftLength(List(1, 2, 3, 4, 5)))
    println(foldLeft(List(1.0, 2.0, 3.0, 4.0, 5.0), 1.0)(_ / _))
    println(foldRight(List(1.0, 2.0, 3.0, 4.0, 5.0), 1.0)(_ / _))
    println(foldRightWithLeft(List(1.0, 2.0, 3.0, 4.0, 5.0), 1.0)(_ / _))
    println(incrementList(List(1, 2, 3, 4, 5)))
    println(mergeIntLists(List(1, 2, 3), List(4, 5, 6)))
    println(hasSubsequence(List(1, 2, 3, 4, 5), List(3, 4)))
  }
}
