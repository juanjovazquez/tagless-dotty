package fo
package step4fin

import fo.step1.{ ExpSym, eval, tf1, P, view }

// type P[Repr] = ExpSym[Repr] ?=> Repr

enum Ctx:
  case Neg, Pos

import Ctx.{ Neg, Pos }

given [Repr] (using s: ExpSym[Repr]) as ExpSym[Ctx => Repr]:
  type R = Ctx => Repr

  def lit(x: Int): R =
    case Pos => s.lit(x)
    case Neg => s.neg(s.lit(x)) 

  def neg(e: R): R =
    case Pos => e(Neg)
    case Neg => e(Pos)

  def add(e1: R, e2: R): R = 
    ctx => s.add(e1(ctx), e2(ctx))

def pushNeg[Repr](e: Ctx => Repr): P[Repr] = e(Pos)

@main def fo_step4fin_main() =
  // tf1 = add(lit(8), neg(add(lit(1), lit(2)))) 

  println(eval(pushNeg(tf1)))
  // 5

  println(view(pushNeg(tf1)))
  // (8 + ((-1) + (-2)))
