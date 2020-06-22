package ho
package step2

// lambda-calculus with integers and addition
// Finally: Tagless final embedding (with de Bruijn indices)

trait Symantics[Repr[_, _]]:
  def int[Env](x: Int): Repr[Env, Int]
  def add[Env](x: Repr[Env, Int], y: Repr[Env, Int]): Repr[Env, Int]

  def vz[Env, A]: Repr[(A, Env), A]
  def vs[Env, A, B](z: Repr[Env, A]): Repr[(B, Env), A]
  def lam[Env, A, B](f: Repr[(A, Env), B]): Repr[Env, A => B]
  def app[Env, A, B](f: Repr[Env, A => B], repa: Repr[Env, A]): Repr[Env, B]

object Symantics:
  def apply[Repr[_, _]](using ev: Symantics[Repr]): Symantics[Repr] = ev

  given Symantics[R]:
    def int[Env](x: Int): R[Env, Int] = 
      R(_ => x)

    def add[Env](x: R[Env, Int], y: R[Env, Int]): R[Env, Int] =
      R(e => x.unR(e) + y.unR(e))

    def vz[Env, A]: R[(A, Env), A] =
      R(_._1)

    def vs[Env, A, B](z: R[Env, A]): R[(B, Env), A] =
      R((_, e) => z.unR(e))

    def lam[Env, A, B](f: R[(A, Env), B]): R[Env, A => B] =
      R(e => a => f.unR(a -> e))

    def app[Env, A, B](f: R[Env, A => B], repa: R[Env, A]): R[Env, B] =
      R(e => f.unR(e)(repa.unR(e)))

  given Symantics[S]:
    def int[Env](x: Int): S[Env, Int] =
      S(_ => x.toString)

    def add[Env](x: S[Env, Int], y: S[Env, Int]): S[Env, Int] =
      S(i => s"(${x.unS(i)} + ${y.unS(i)})")

    def vz[Env, A]: S[(A, Env), A] =
      S(i => s"x${i - 1}")

    def vs[Env, A, B](z: S[Env, A]): S[(B, Env), A] =
      S(i => z.unS(i - 1))

    def lam[Env, A, B](f: S[(A, Env), B]): S[Env, A => B] = 
      S(i => s"(\\\\x$i -> ${f.unS(i + 1)})")

    def app[Env, A, B](f: S[Env, A => B], repa: S[Env, A]): S[Env, B] =
      S(i => s"(${f.unS(i)} ${repa.unS(i)})")
    
  object syntax:
    def int[Repr[_, _], Env](x: Int)(using Symantics[Repr]): Repr[Env, Int] =
      Symantics[Repr].int(x)

    def add[Repr[_, _], Env](x: Repr[Env, Int], y: Repr[Env, Int])
        (using Symantics[Repr]): Repr[Env, Int] =
      Symantics[Repr].add(x, y)

    def vz[Repr[_, _], Env, A](using Symantics[Repr]): Repr[(A, Env), A] =
      Symantics[Repr].vz

    def vs[Repr[_, _], Env, A, B](z: Repr[Env, A])(using Symantics[Repr]): Repr[(B, Env), A] =
      Symantics[Repr].vs(z)

    def lam[Repr[_, _], Env, A, B](f: Repr[(A, Env), B])(using Symantics[Repr]): Repr[Env, A => B] =
      Symantics[Repr].lam(f)

    def app[Repr[_, _], Env, A, B](f: Repr[Env, A => B], repa: Repr[Env, A])
        (using Symantics[Repr]): Repr[Env, B] =
      Symantics[Repr].app(f, repa)

import Symantics.syntax._

type P[Repr[_, _], Env, A] = Symantics[Repr] ?=> Repr[Env, A]

def td1[Repr[_, _]]: P[Repr, Unit, Int] = 
  add(int(1), int(2))

def td2o[Repr[_, _]]: P[Repr, (Int, Unit), Int => Int] = 
  lam(add(vz, vs(vz[Repr, Unit, Int])))

def td3[Repr[_, _]]: P[Repr, Unit, (Int => Int) => Int] =
  lam(add(app(vz, int(1)), int(2)))

def eval[A](e: R[Unit, A]) = e.unR(())
def view[A](e: S[Unit, A]) = e.unS(0)

@main def ho_step2_main() =
  println(eval(td1))
  // 3
  
  val rd3: (Int => Int) => Int = eval(td3)
  println(rd3(_ + 1))
  // 4 

  //val rd2o = eval(td2o)
  // `td2o` is an open term so `eval(td2o)`
  // is ill-typed.

  println(view(td1))
  // (1 + 2)

  println(view(td3))
  // (\\x0 -> ((x0 1) + 2))