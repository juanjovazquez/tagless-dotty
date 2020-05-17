package ho
package step0

// lambda-calculus with booleans
// First attempt: Untyped and tagged

enum Exp:
  case V(v: Var)
  case B(b: Boolean)
  case L(f: Exp)
  case A(f: Exp, arg: Exp)

// variables as `de Bruijn` indices
enum Var:
  case VZ
  case VS(v: Var)

import Exp._, Var._

val ti1 = A(L(V(VZ)), B(true)) // ((b: Boolean) => b)(true)

// we try a naive evaluator
type Env0 = List[Boolean]

def lookup0(v: Var, env: Env0): Boolean =
  (v, env) match
    case (VZ, x :: _)       => x
    case (VS(v), _ :: env0) => lookup0(v, env0)
    case _                  => sys.error("not valid environment")

// Does not type check!

//def eval0(e: Exp, env: Env) = (env, e) match
//  case (env0, V(v))  => lookup(v, env0) // Boolean
//  case (env0, B(b))  => b // Boolean
//  case (env0, L(e0)) => x => eval0(e0, env0) // Scala function value
//  case (env0, A(f, arg)) => (eval0(f, env0))(eval0(arg, env0))

// The second branch returns `Booolean` whereas the next one 
// returns a Scala function value, i.e. `eval0` is ill-typed.

// We have little choice but introduce a universal type 
// as the union of booleans and functions through tags:
enum U:
  case UB(b: Boolean)
  case UA(f: U => U)

import U._

type Env = List[U]
def lookup(v: Var, env: Env): U =
  (v, env) match
    case (VZ, x :: _)       => x
    case (VS(v), _ :: env0) => lookup(v, env0)
    case _                  => sys.error("Impossible!")

def eval(e: Exp, env: Env): U = (env, e) match
  case (env0, V(v))      => lookup(v, env)
  case (env0, B(b))      => UB(b)
  case (env0, L(e0))     => UA(x => eval(e0, x :: env0))
  case (env0, A(f, arg)) => 
    eval(f, env0) match
      case UA(f0) => f0(eval(arg, env0))
      case UB(_)  => sys.error("Impossible!") // not exhaustive

@main def ho_step0_main() =
  println(eval(ti1, Nil))
  // UB(true) 