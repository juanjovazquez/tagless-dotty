package fo
package step4ini

import fo.step0.Exp
import fo.step0.Exp.{ Add, Lit, Neg}
import fo.step0.{ eval, ti1, view }

// More restrictive grammar (only integers can be negated and only once):
// ---
// e      ::= factor | add e e
// factor ::= int | neg int

// initial approach
def pushNeg(e: Exp): Exp = e match
  case Lit(_)           => e
  case Neg(Lit(_))      => e
  case Neg(Neg(e))      => pushNeg(e)
  case Neg(Add(e1, e2)) => Add(pushNeg(Neg(e1)), pushNeg(Neg(e2)))
  case Add(e1, e2)      => Add(pushNeg(e1), pushNeg(e2))

val ti1Norm = pushNeg(ti1)

@main def fo_step4ini_main() =
  println(eval(ti1Norm))
  // 5

  println(view(ti1Norm))
  // (8 + ((-1) + (-2)))