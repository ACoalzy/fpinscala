package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r2) = nonNegativeInt(rng)
    val (d, r3) = double(r2)
    ((i, d), r3)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r2) = nonNegativeInt(rng)
    (i.toDouble / Integer.MAX_VALUE, r2)
  }

  def doubleMap(rng: RNG): Rand[Double] = map(nonNegativeInt)(i => i.toDouble / Integer.MAX_VALUE)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, r2) = rng.nextInt
    if (v > 0) return (v, r2)
    else return (-(v + 1), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r2) = double(rng)
    val (i, r3) = nonNegativeInt(r2)
    ((d, i), r3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r2) = double(rng)
    val (d2, r3) = double(r2)
    val (d3, r4) = double(r3)
    ((d1, d2, d3), r4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(n: Int, acc: List[Int], r: RNG): (List[Int], RNG) = {
      if (n > 0) {
        val (v, r2) = nonNegativeInt(r)
        go(n - 1, v :: acc, r2)
      } else {
        (acc, r)
      }
    }

    go(count, List(), rng)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r) = ra(rng)
    val (b, r2) = rb(r)
    (f(a, b), r2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    def go(l: List[Rand[A]], acc: List[A], r: RNG): (List[A], RNG) = l match {
      case Nil => (acc, r)
      case h :: t => {
        val (v, r2) = h(r)
        go(t, v :: acc, r2)
      }
    }

    go(fs, List(), rng)
  }

  def sequenceFold[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((h, t) => map2(h, t)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  def mapFM[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2FM[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = for (
    a <- ra;
    b <- rb
  ) yield f(a, b)

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(a => {
    val mod = a % n
    if (a + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  })

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = for (
    a <- this;
    b <- sb
  ) yield f(a, b)

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] = ss.foldRight(unit[S, List[A]](List()))((h, t) => h.map2(t)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def update(i: Input)(s: Machine): Machine = i match {
    case Coin if (s.locked && s.candies > 0) => Machine(false, s.candies, s.coins + 1)
    case Turn if (!s.locked) => Machine(true, s.candies - 1, s.coins)
    case _ => s
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for (
    //_ <- sequence(inputs map (modify[Machine] _ compose update))
    _ <- sequence(inputs.map(i => modify[Machine](s => update(i)(s))));
    s <- get
  ) yield (s.coins, s.candies)
}
