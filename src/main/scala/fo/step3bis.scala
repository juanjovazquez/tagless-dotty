package fo
package step3bis

import step1.ExpSym, step2.MulSym

trait Expr:
  self =>

  def apply[Repr: ExpSym]: Repr

  def unary_- : Expr = new :
    def apply[Repr: ExpSym]: Repr = ExpSym[Repr].neg(self[Repr])

  def +(that: Expr): Expr = new :
    def apply[Repr: ExpSym]: Repr = ExpSym[Repr].add(self[Repr], that[Repr])

  def -(that: Expr): Expr = new :
    def apply[Repr: ExpSym]: Repr = ExpSym[Repr].add(self[Repr], (- that)[Repr])

object Expr:
  def lit(x: Int): Expr = new :
    def apply[Repr: ExpSym]: Repr = ExpSym[Repr].lit(x)

  // implicit conversion is done via `given` now 
  given Conversion[Int, Expr] = lit(_)

// extension method
def (x: Int).lit: Expr = Expr.lit(x)

//def tf1[Repr]: P[Repr] = 
//  add(lit(8), neg(add(lit(1), lit(2)))) 

import Expr.{ _, given _ }

val tf1bis = 8 - (1 + 2.lit)

@main def step3bis_main() = {
  println(tf1bis[Int])
  println(tf1bis[String])
}
