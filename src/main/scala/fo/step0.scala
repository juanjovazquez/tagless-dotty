package fo
package step0

// initial embedding
enum Exp:
  case Lit(x: Int)
  case Neg(e: Exp)
  case Add(e1: Exp, e2: Exp)

import Exp._

val ti1 = 
  Add(Lit(8), Neg(Add(Lit(1), Lit(2)))) // 8 + (- (1 + 2))

def eval(e: Exp): Int = 
  e match
    case Lit(x)      => x
    case Neg(e)      => - eval(e)
    case Add(e1, e2) => eval(e1) + eval(e2)

// final embedding (metacircular interpreter)
type Repr = Int

def lit(x: Int): Repr             = x
def neg(e: Repr): Repr            = - e
def add(e1: Repr, e2: Repr): Repr = e1 + e2

val tf1 = add(lit(8), neg(add(lit(1), lit(2))))

// the initial embedding seems more general 
// permitting other interpretations,
// e.g. a pretty-printer
def view(e: Exp): String = 
  e match
    case Lit(x)      => x.toString
    case Neg(e)      => s"(-${ view(e) })"
    case Add(e1, e2) => s"(${ view(e1) } + ${ view(e2) })"

// we need to parametrize the 'constructor functions'

@main def fo_step0_main() =
  println(eval(ti1))
  // 5

  println(view(ti1))
  // (8 + (-(1 + 2)))