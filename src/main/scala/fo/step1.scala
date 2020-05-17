package fo
package step1

trait ExpSym[Repr]:
  def lit(x: Int): Repr
  def neg(e: Repr): Repr
  def add(e1: Repr, e2: Repr): Repr

// `ExpSym` is the denotational semantics over
// the semantic domain `Repr`: the meaning of
// an expression is computed from the meaning
// of the components (regardless `Repr`)

object ExpSym:
  inline def apply[Repr](using sym: ExpSym[Repr]): ExpSym[Repr] = sym

  given ExpSym[Int]:
    def lit(x: Int): Int           = x
    def neg(e: Int): Int           = - e
    def add(e1: Int, e2: Int): Int = e1 + e2
  
  given ExpSym[String]:
    def lit(x: Int): String                 = x.toString
    def neg(e: String): String              = s"(-$e)"
    def add(e1: String, e2: String): String = s"($e1 + $e2)"

// `Sym` stands for 'Symantics': the type class declaration
// defines the syntax; class instances define interpretations
// i.e. its semantics

// some syntax to code terms easier
object syntax:
  def lit[Repr](x: Int)(using sym: ExpSym[Repr]) = 
    sym.lit(x)
  def neg[Repr](e: Repr)(using sym: ExpSym[Repr]) = 
    sym.neg(e)
  def add[Repr](e1: Repr, e2: Repr)(using sym: ExpSym[Repr]) = 
    sym.add(e1, e2)

import syntax._

// the type of the expressions built
// with the syntax of the embedded language
// (uses new 'context function types') 
type P[Repr] = ExpSym[Repr] ?=> Repr

def tf1[Repr]: P[Repr] = 
  add(lit(8), neg(add(lit(1), lit(2)))) 

// trivial evaluators (basically identity functions);
// the 'real' evaluation process is somehow hardwired
// to the interpreter
def eval(x: Int): Int       = x 
def view(s: String): String = s

//eval(tf1)
//view(tf1)