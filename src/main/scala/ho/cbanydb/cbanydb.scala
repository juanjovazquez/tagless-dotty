package ho.cbanydb

type Arr[Rep[_, _], E, A, B] = Rep[E, A] => Rep[E, B]

trait Symantics[Rep[_, _]]:
  def int[E](x: Int): Rep[E, Int]
  def add[E](x: Rep[E, Int], y: Rep[E, Int]): Rep[E, Int]
  def sub[E](x: Rep[E, Int], y: Rep[E, Int]): Rep[E, Int]
  def vz[E, A]: Rep[(A, E), A]
  def vs[E, A, B](vz: Rep[E, A]): Rep[(B, E), A]
  def app[E, A, B](f: Rep[E, Arr[Rep, E, A, B]], x: Rep[E, A]): Rep[E, B]

trait SymLam[Rep[_, _]]:
  def lam[E, A, B](f: Rep[(E, A), B]): Rep[E, Arr[Rep, E, A, B]]

def t0[Rep[_, _], E](using sym: Symantics[Rep]): Rep[E, Int] =
  import sym._
  add(int(1), int(1))


//@main def CBAnyDB() = 
