package fo
package step2

import step1.{ ExpSym, eval, view }
import step1.syntax._

trait MulSym[Repr]:
  def mul(x: Repr, y: Repr): Repr

def mul[Repr](x: Repr, y: Repr)(using mulsym: MulSym[Repr]): Repr =
  mulsym.mul(x, y)

type P[Repr] = MulSym[Repr] ?=> step1.P[Repr]

// the new `mul` appears alongside the old
// forms `lit`, `neg` and `add` 
def tfm1[Repr]: P[Repr] =
  add(lit(7), neg(mul(lit(2), lit(2))))

// incorporates with no changes the term
// `step1.tf1` from the unextended language
def tfm2[Repr]: P[Repr] =
  mul(lit(7), step1.tf1)

object MulSym:
  inline def apply[Repr](using ev: MulSym[Repr]): MulSym[Repr] = ev

  given MulSym[Int]:
    def mul(x: Int, y: Int): Int = 
      x * y

  given MulSym[String]: 
    def mul(x: String, y: String): String =
      s"($x * $y)"

// The final encoding makes it easy to add not 
// only new interpretations but also new language
// forms making the interpreters extensible by default.

@main def fo_step2_main() =
  println(eval(tfm1))
  // 3

  println(eval(tfm2))
  // 35

  println(view(tfm1))
  // (7 + (-(2 * 2)))

  println(view(tfm2))
  // (7 * (8 + (-(1 + 2))))