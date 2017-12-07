package fpinscala.testing

import fpinscala.state.{RNG, State}
import fpinscala.testing.Prop._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => {
      this.run(max, n, rng) match {
        case Passed => p.run(max, n, rng)
        case f => f
      }
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => {
      this.run(max, n, rng) match {
        case Falsified(_, _) => p.run(max, n, rng)
        case p => p
      }
    }
  }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def unfold[A, B](start: B)(f: B => Option[(A, B)]): Stream[A] = f(start) match {
    case Some((elem, next)) => elem #:: unfold(next)(f)
    case None => Stream.empty
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

}

case class Gen[+A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(Gen.listOfN(_, this))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeLessThan(2)).map(i => i == 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap((b: Boolean) => if (b) g1 else g2)

  def choose(max: Double): Gen[Double] = Gen(State(RNG.double).map(d => d % max))

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = Gen(State(RNG.double).flatMap(d => {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    if (d < threshold) g1._1.sample else g2._1.sample
  }))
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen {
    n => forSize(n).flatMap(f(_).forSize(n))
  }
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n.max(1), g))
}

object Tester {

  def main(args: Array[String]): Unit = {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(SGen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    Prop.run(maxProp)

    val sortedProp = Prop.forAll(SGen.listOf1(smallInt)) { ns =>
      val sorted = ns.sorted
      !sorted.exists(!ns.contains(_)) &&
      !ns.exists(!sorted.contains(_)) &&
      sorted.foldRight((Integer.MAX_VALUE, true))((x, y) => (x, x <= y._1 && y._2))._2
    }
    Prop.run(sortedProp)
  }
}