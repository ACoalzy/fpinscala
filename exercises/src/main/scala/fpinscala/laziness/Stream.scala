package fpinscala.laziness

import Stream._
import com.sun.crypto.provider.AESCipher.AES128_CBC_NoPadding

import scala.collection.mutable.ListBuffer
trait Stream[+A] {

    def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toListFast: List[A] = {
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h,t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
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
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => cons(h(), t().take(n-1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => t().drop(n-1)
    case _ => _
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def takeWhileFR(p: A => Boolean): Stream[A] =
    this.foldRight(empty[A])((h, t) => if (p(h)) cons(h,t) else empty)

  def headOption: Option[A] = this.foldRight(None:Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = this.foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = this.foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B>:A](s2: => Stream[B]): Stream[B] = this.foldRight(s2)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(empty[B])((a, b) => f(a).append(b))

  def mapUnfold[B](f: A => B): Stream[B] = unfold(this){
    case Cons(h, t) => Some(f(h), t())
    case _ => None
  }

  def takeUnfold(n: Int): Stream[A] = unfold(this){
    case Cons(h, t) if (n > 0) => Some(h(), t())
    case _ => None
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if (p(h)) => Some(h(), t())
    case _ => None
  }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = unfold(this, s) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this, s) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h, t), _) => Some((Some(h()), None), (t(), empty))
    case (_, Cons(h, t)) => Some((None, Some(h())), (empty, t()))
    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean = this.zipAll(s).forAll {
    case (Some(a), Some(b)) => a == b
    case (None, Some(b)) => false
    case _ => true
  }

  def tails: Stream[Stream[A]] = unfold(this)(s => s match {
    case Cons(_, t) => Some(s, t())
    case Empty => None
  }).append(empty)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    this.foldRight(z, Stream(z))((a, b) => {
      lazy val p = b
      val fp = f(a, p._1)
      (fp, Stream(fp).append(p._2))
    })._2
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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a+b))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, b)) => cons(a, unfold(b)(f))
      case _ => empty
    }

  val onesUnfold: Stream[Int] = unfold(1)(a => Some(a, a))

  def constantUnfold[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s+1))

  // could use match (s1, s2) => Some(s1, (s2, s1+s2))
  def fibsUnfold: Stream[Int] = unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))
}