package ho
package step1

// lambda-calculus with booleans
// Second attempt: Tagless initial embedding

// The initial approach represents terms of the
// embedded language as values of an algebraic
// data type. GADT support in Scala is sufficient
// for the initial encoding.

enum Var[Env, T]:
  case VZ[Env, T]()                  extends Var[(T, Env), T]
  case VS[Env, A, T](v: Var[Env, T]) extends Var[(A, Env), T]

enum Exp[Env, T]:
  case B[Env](b: Boolean)                                extends Exp[Env, Boolean]
  case V[Env, T](v: Var[Env, T])                         extends Exp[Env, T]
  case L[Env, A, B](f: Exp[(A, Env), B])                 extends Exp[Env, A => B]
  case A[Env, A, B](f: Exp[Env, A => B], e: Exp[Env, A]) extends Exp[Env, B]

import Var._, Exp._

val ti1: Exp[Unit, Boolean] = A(L(V(VZ())), B(true)) // ((b: Boolean) => b)(true)

// The Scala compiler asks for exhaustivity even though 
// this function is total now
def lookp[Env, T](v: Var[Env, T], env: Env): T = (v, env) match
  case (VZ(), (x, _))     => x
  case (VS(v), (_, env0)) => lookp(v, env0)
  case (_, _)             => sys.error("Impossible!")

def eval[Env, T](env: Env, e: Exp[Env, T]): T = (env, e) match
  case (ev, V(v)) => 
    lookp(v, ev)

  case (_, B(b)) => 
    b

  case (ev, L(f)) => 
    def aux[Env0, A, B](f0: Exp[(A, Env0), B], ev0: Env0) = 
      (x: A) => eval((x, ev0), f0)
    aux(f, ev)
  
  case (ev0, A(f, e)) => 
    (eval(ev0, f))(eval(ev0, e))

@main def ho_step1_main() =
  println(eval((), ti1))
  // true