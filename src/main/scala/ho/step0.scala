package ho
package step0

import scala.util.{ Left, Right }

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

//def eval0(e: Exp, env: Env) = e match
//  case V(v)  => lookup(v, env) // Boolean
//  case B(b)  => b // Boolean
//  case L(e0) => x => eval0(e0, x :: env) // Scala function value
//  case A(f, arg) => (eval0(f, env))(eval0(arg, env))

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
    //case _                  => sys.error("Impossible!")

def eval(e: Exp, env: Env): U = e match
  case V(v)      => lookup(v, env)
  case B(b)      => UB(b)
  case L(e0)     => UA(x => eval(e0, x :: env))
  case A(f, arg) => 
    eval(f, env) match
      case UA(f0) => f0(eval(arg, env))
      case UB(_)  => sys.error("Impossible!") // not exhaustive

// Unfortunately, `eval` is partial and fails evaluating
// the term:
val ti2a = A(B(true), B(false)) // app(true, false) !!

// Also `lookup` is partial and accepts open terms as:
val ti2o  = A(L(V(VS(VZ))), B(true)) // ... instead of:
val ti2ok = A(L(V(VZ)), B(true))

// The object language is untyped so errors may occur
// during evaluation. To prevent erros we need:

type ErrMsg = String
def typecheck(e: Exp): Either[ErrMsg, Exp] = ???

def safeEval(e: Exp) = typecheck(e) match
  case Right(x) => println(eval(x, Nil))
  case Left(t)  => println(s"Type error: $t")

// The presence of type tags (UB, UA) and the need
// for runtime checking reveal the lack of type safety.
// It's possible to create ill terms. The embedding is
// not "tight". We failed.

@main def ho_step0_main() =
  println(eval(ti1, Nil))
  // UB(true) 

  //println(eval(ti2a, Nil))
  // java.lang.RuntimeException: Impossible!

  //println(eval(ti2o, Nil))
  // java.lang.RuntimeException: Impossible!

  println(eval(ti2ok, Nil))
  // UB(true)