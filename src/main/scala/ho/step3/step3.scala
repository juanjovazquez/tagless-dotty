package ho
package step3

// Extensibility by enriching the language:
// PCF: lambda calculus with integer and booleans operations,
// the conditional and the fixpoint.
// Finally: Tagless final embedding with HOAS (Higher-Order Abstract Syntax)

trait Symantics[Repr[_]]:
  def int(x: Int): Repr[Int]
  def add(x: Repr[Int], y: Repr[Int]): Repr[Int]

  def lam[A, B](f: Repr[A] => Repr[B]): Repr[A => B]
  def app[A, B](f: Repr[A => B], repa: Repr[A]): Repr[B]

object Symantics:
  def apply[Repr[_]](using ev: Symantics[Repr]): Symantics[Repr] = ev

  given Symantics[R]:
    def int(x: Int): R[Int] = 
      R(x)

    def add(x: R[Int], y: R[Int]): R[Int] =
      R(x.unR + y.unR)

    def lam[A, B](f: R[A] => R[B]): R[A => B] =
      R(a => f(R(a)).unR)

    def app[A, B](f: R[A => B], repa: R[A]): R[B] =
      R(f.unR(repa.unR))

  given Symantics[S]:
    def int(x: Int): S[Int] = 
      S(_ => x.toString)

    def add(x: S[Int], y: S[Int]): S[Int] =
      S(i => s"(${x.unS(i)} + ${y.unS(i)})")

    def lam[A, B](f: S[A] => S[B]): S[A => B] =
      S { i =>
        val x = s"x$i"
        s"(\\\\$x -> ${f(S(_ => x)).unS(i + 1)})"
      }

    def app[A, B](f: S[A => B], repa: S[A]): S[B] =
      S(i => s"(${f.unS(i)} ${repa.unS(i)})")
    
  object syntax:
    def int[Repr[_]: Symantics](x: Int): Repr[Int] =
      Symantics[Repr].int(x)

    def add[Repr[_]: Symantics](x: Repr[Int], y: Repr[Int]): Repr[Int] =
      Symantics[Repr].add(x, y)

    def lam[Repr[_]: Symantics, A, B](f: Repr[A] => Repr[B]): Repr[A => B] =
      Symantics[Repr].lam(f)

    def app[Repr[_]: Symantics, A, B](f: Repr[A => B], repa: Repr[A]): Repr[B] = 
      Symantics[Repr].app(f, repa)

    def [Repr[_]: Symantics, A, B](f: Repr[A => B]).ap(repa: Repr[A]): Repr[B] = 
      Symantics[Repr].app(f, repa)

import Symantics.syntax._

type P[Repr[_], A] = Symantics[Repr] ?=> Repr[A]

def th1[Repr[_]]: P[Repr, Int] = 
  add(int(1), int(2))

def th2[Repr[_]]: P[Repr, Int => Int] =
  lam(x => add(x, x))

def th3[Repr[_]]: P[Repr, (Int => Int) => Int] =
  lam(x => add(app(x, (int(1))), int(2)))

def eval[A](e: R[A]): A = e.unR
def view[A](e: S[A]): String = e.unS(0)

trait MulSym[Repr[_]]:
  def mul(x: Repr[Int], y: Repr[Int]): Repr[Int]

object MulSym:
  def apply[Repr[_]](using ev: MulSym[Repr]): MulSym[Repr] = ev

  given MulSym[R]:
    def mul(x: R[Int], y: R[Int]): R[Int] =
      R(x.unR * y.unR)

  given MulSym[S]:
    def mul(x: S[Int], y: S[Int]): S[Int] =
      S(i => s"(${x.unS(i)} * ${y.unS(i)})")

  object syntax:
    def mul[Repr[_]: MulSym](x: Repr[Int], y: Repr[Int]): Repr[Int] =
      MulSym[Repr].mul(x, y)

trait BoolSym[Repr[_]]:
  def bool(x: Boolean): Repr[Boolean]
  def leq(x: Repr[Int], y: Repr[Int]): Repr[Boolean]
  def if_[A](p: Repr[Boolean], e1: => Repr[A], e2: => Repr[A]): Repr[A]

object BoolSym:
  def apply[Repr[_]](using ev: BoolSym[Repr]): BoolSym[Repr] = ev

  given BoolSym[R]:
    def bool(x: Boolean): R[Boolean] =
      R(x)

    def leq(x: R[Int], y: R[Int]): R[Boolean] =
      R(x.unR <= y.unR)

    def if_[A](p: R[Boolean], e1: => R[A], e2: => R[A]): R[A] =
      R(if p.unR then e1.unR else e2.unR)

  given BoolSym[S]:
    def bool(x: Boolean): S[Boolean] =
      S(_ => if x then "True" else "False")

    def leq(x: S[Int], y: S[Int]): S[Boolean] =
      S(i => s"(${x.unS(i)} <= ${y.unS(i)})")

    def if_[A](p: S[Boolean], e1: => S[A], e2: => S[A]): S[A] =
      S(i => s"(if ${p.unS(i)} then ${e1.unS(i)} else ${e2.unS(i)})")
  
  object syntax:
    def bool[Repr[_]: BoolSym](x: Boolean): Repr[Boolean] =
      BoolSym[Repr].bool(x)

    def leq[Repr[_]: BoolSym](x: Repr[Int], y: Repr[Int]): Repr[Boolean] =
      BoolSym[Repr].leq(x, y)

    def if_[Repr[_]: BoolSym, A](p: Repr[Boolean])(e1: => Repr[A])(e2: => Repr[A]): Repr[A] =
      BoolSym[Repr].if_(p, e1, e2)

trait FixSym[Repr[_]]:
  def fix[A](f: ( => Repr[A]) => Repr[A]): Repr[A]

object FixSym:
  def apply[Repr[_]](using ev: FixSym[Repr]): FixSym[Repr] = ev

  given FixSym[R]:
    def fix[A](f: (=> R[A]) => R[A]): R[A] =
      def fx(f: (=> R[A]) => R[A]): R[A] =
        lazy val ra: R[A] = f(ra)
        ra
      R(fx(f).unR)

  given FixSym[S]:
    def fix[A](f: (=> S[A]) => S[A]): S[A] =
      S { i =>
        val self = s"self$i"
        s"(fix $self . ${f(S(_ => self)).unS(i + 1)})"
      }

  object syntax:
    def fix[Repr[_]: FixSym, A](f: (=> Repr[A]) => Repr[A]): Repr[A] =
      FixSym[Repr].fix(f)

import MulSym.syntax._, BoolSym.syntax._, FixSym.syntax._

type PE[Repr[_], A] = (Symantics[Repr], MulSym[Repr], BoolSym[Repr], FixSym[Repr]) ?=> Repr[A]

def tpow[Repr[_]]: PE[Repr, Int => Int => Int] =
  lam { x =>
    fix(self => lam { n =>
      if_ (leq(n, int(0))) 
          (int(1)) 
          (mul(x, app(self, add(n, int(-1)))))
    })
  }

def tpow7[Repr[_]]: PE[Repr, Int => Int] =
  lam(x => (tpow ap x) ap int(7))

def tpow72[Repr[_]]: PE[Repr, Int] = 
  tpow7 ap int(2)

@main def ho_step3_main() =
  println(eval(th1))
  // 3
  
  val rh2: Int => Int = eval(th2)
  println(rh2(1))
  // 2

  val rh3: (Int => Int) => Int = eval(th3)
  println(rh3(_ + 1))
  // 4

  println(view(th1))
  // (1 + 2)

  println(view(th2))
  // (\\x0 -> (x0 + x0))

  println(view(th3))
  // (\\x0 -> ((x0 1) + 2))

  println(eval(tpow72))
  // 128

  println(view(tpow))
  // (\\x0 -> (fix self1 . (\\x2 -> (if (x2 <= 0) then 1 else (x0 * (self1 (x2 + -1)))))))