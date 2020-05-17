package ho.cbany

import cats.{ Monad, Show }
import cats.implicits._
import cats.effect.{ IO, LiftIO, Sync }
import cats.effect.concurrent.Ref
import scala.annotation.infix

type Arr[Rep[_], A, B] = Rep[A] => Rep[B]

trait Symantics[Rep[_]]:
  def int(x: Int): Rep[Int]
  def add(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def sub(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def app[A, B](f: Rep[Arr[Rep, A, B]], x: Rep[A]): Rep[B]

trait SymLam[Rep[_]]:
  def lam[A, B](f: Rep[A] => Rep[B]): Rep[Arr[Rep, A, B]]

object syntax:
  def int[Rep[_]](x: Int)(using sym: Symantics[Rep]): Rep[Int] =
    sym.int(x)

  @infix def [Rep[_]](x: Rep[Int]).add(y: Rep[Int])(using sym: Symantics[Rep]): Rep[Int] =
    sym.add(x, y)

  @infix def [Rep[_]](x: Rep[Int]).sub(y: Rep[Int])(using sym: Symantics[Rep]): Rep[Int] =
    sym.sub(x, y)

  @infix def [Rep[_], A, B](f: Rep[Arr[Rep, A, B]]).app(x: Rep[A])(using sym: Symantics[Rep]): Rep[B] =
    sym.app(f, x)

  def lam[Rep[_], A, B](f: Rep[A] => Rep[B])(using symL: SymLam[Rep]): Rep[Arr[Rep, A, B]] =
    symL.lam(f)

  def let[Rep[_]: Symantics: SymLam, A, B](x: Rep[A], f: Rep[A] => Rep[B]): Rep[B] =
    lam(f) app x

  def putStrLn(str: String) = IO(println(str))

  def print[A: Show](x: A) = putStrLn(x.show)

import syntax._

type R[Rep[_], A] = (Symantics[Rep], SymLam[Rep]) ?=> Rep[A]

// (\z x -> let y = x + x in y + y) (100 - 10) (5 + 5)
// `z` does not occur in the body of the abstraction whereas
// `x` occurs twice.
def t2[Rep[_]]: R[Rep, Int] =
  lam((z: Rep[Int]) => lam((x: Rep[Int]) => let(x add x, y => y add y))) app 
    (int(100) sub int(10)) app 
    (int(5) add int(5))

object Symantics:
  given SSymantics[L, M[_]: Monad: LiftIO] as Symantics[S1[L, M]]:
    type R[A] = S[L, M, A]

    def int(x: Int): R[Int] = 
      x.pure[R]

    def add(x: R[Int], y: R[Int]): R[Int] = 
      for
        x0 <- x
        y0 <- y
        _  <- putStrLn("Adding").to[R]
      yield x0 + y0

    def sub(x: R[Int], y: R[Int]): R[Int] = 
      for
        x0 <- x
        y0 <- y
        _  <- putStrLn("Subtracting").to[R]
      yield x0 - y0

    def app[A, B](f: R[Arr[R, A, B]], x: R[A]): R[B] =
      f.flatMap(_(x))

trait Name   // call-by-name
trait Value  // call-by-value
trait Lazy   // call-by-need 

object SymLam:
  given NameSSymLam[M[_]: Monad] as SymLam[S1[Name, M]]:
    type R[A] = S[Name, M, A]
    
    // the function is lifted without prior evaluation (call-by-name)
    def lam[A, B](f: R[A] => R[B]): R[Arr[R, A, B]] = f.pure

  given ValueSSymLam[M[_]: Monad] as SymLam[S1[Value, M]]:
    type R[A] = S[Value, M, A]

    // the lambda abstraction receives an argument `ra` 
    // evaluating it before passing its result to `f`
    // (i.e. call-by-value)
    def lam[A, B](f: R[A] => R[B]): R[Arr[R, A, B]] = 
      ((ra: R[A]) => ra.flatMap(f compose Monad[R].pure)).pure

  given LazySSymLam[M[_]: Sync] as SymLam[S1[Lazy, M]]:
    type R[A] = S[Lazy, M, A]
    
    // the evaluation of the body starts by lazy sharing the
    // argument expression (i.e. call-by-need). Application
    // arguments are evaluated at most once
    def lam[A, B](f: R[A] => R[B]): R[Arr[R, A, B]] = 
      ((ra: R[A]) => memoize(ra).flatMap(f)).pure

    private def memoize[M[_]: Sync, A](ma: M[A]): M[M[A]] =
      Ref.of[M, (Boolean, M[A])](false -> ma).map(cache =>
        for
          tpl <- cache.get
          (flag, ma) = tpl
          out <- if flag then ma 
                 else 
                    for
                      v <- ma
                      _ <- cache.set(true -> v.pure)
                    yield v
        yield out)

def runName[M[_], A](x: S[Name, M, A]): M[A]   = S.unS(x)
def runValue[M[_], A](x: S[Value, M, A]): M[A] = S.unS(x)
def runLazy[M[_], A](x: S[Lazy, M, A]): M[A]   = S.unS(x)

@main def program() =
  println("=== call-by-name ===")
  type N[A] = S[Name, IO, A]
  (runName(t2[N]) >>= print).unsafeRunSync
  
  
  println("=== call-by-value ===")
  type V[A] = S[Value, IO, A]
  (runValue(t2[V]) >>= print).unsafeRunSync

  println("=== call-by-need ===")
  type L[A] = S[Lazy, IO, A]
  (runLazy(t2[L]) >>= print).unsafeRunSync
