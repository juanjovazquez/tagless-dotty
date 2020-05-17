package fo
package step3

import step1.ExpSym
import step2.MulSym
import scala.language.postfixOps
import scala.util.{ Try, Right, Left }

type ErrMsg = String

enum Tree:
  case Leaf(lbl: String)
  case Node(lbl: String, ts: List[Tree])

object Tree:
  given ExpSym[Tree]:
    def lit(x: Int): Tree = 
      Node("Lit", Leaf(x.toString) :: Nil)

    def neg(e: Tree): Tree =
      Node("Neg", e :: Nil)

    def add(e1: Tree, e2: Tree): Tree =
      Node("Add", e1 :: e2 :: Nil)

  given MulSym[Tree]:
    def mul(e1: Tree, e2: Tree): Tree =
      Node("Mul", e1 :: e2 :: Nil)
  
  object Lit:
    def unapply(t: Tree): Option[Int] =
      t match
        case Node("Lit", Leaf(s) :: Nil) => Try(s.toInt).toOption
        case _                           => None

  object Neg:
    def unapply(t: Tree): Option[Tree] =
      t match
        case Node("Neg", t :: Nil) => Some(t)
        case _                     => None

  object Add:
    def unapply(t: Tree): Option[(Tree, Tree)] =
      t match
        case Node("Add", l :: r :: Nil) => Some(l -> r)
        case _                          => None

  object Mul:
    def unapply(t: Tree): Option[(Tree, Tree)] =
      t match
        case Node("Mul", l :: r :: Nil) => Some(l -> r)
        case _                          => None

import Tree.{ Lit, Neg, Add, Mul }

def fromTree[Repr: ExpSym](t: Tree): Either[ErrMsg, Repr] =
  t match
    case Lit(x) => 
      Right(ExpSym[Repr].lit(x))
    case Neg(t) => 
      fromTree(t).map(ExpSym[Repr].neg)
    case Add(l, r) => 
      for 
        l0 <- fromTree(l)
        r0 <- fromTree(r)
      yield ExpSym[Repr].add(l0, r0)
    case t => Left(s"Invalid tree: $t")

// We lost polymorphism...

import step1.ExpSym, step1.{ P => P1 }, step2.{ P => P2 }

// `newtype Expr = Expr (forall repr. ExpSym repr => repr)`
trait Expr:
  def apply[Repr]: P2[Repr]

object Expr:
  given ExpSym[Expr]:
    def lit(x: Int): Expr = new :
      def apply[Repr]: P2[Repr] = ExpSym[Repr].lit(x)

    def neg(e: Expr): Expr = new :
      def apply[Repr]: P2[Repr] = ExpSym[Repr].neg(e[Repr])

    def add(e1: Expr, e2: Expr): Expr = new :
      def apply[Repr]: P2[Repr] = ExpSym[Repr].add(e1[Repr], e2[Repr])

// Extensibiity in the parser
trait OpenInterpreter[A, B, E]:
  self => // really awkward

  def apply(recur: => A => Either[E, B]): A => Either[E, B]

  def orElse(that: OpenInterpreter[A, B, E]): OpenInterpreter[A, B, E] = new :
    def apply(f: => A => Either[E, B]): A => Either[E, B] =
      a => self(f)(a) orElse that(f)(a)

  def close: A => Either[E, B] = fix(apply)

  private[this] def fix[A](f: (=> A) => A): A =
    lazy val a: A = f(a)
    a

class ExpSymInterpreter[Repr: ExpSym] extends OpenInterpreter[Tree, Repr, ErrMsg]:
  def apply(recur: => Tree => Either[ErrMsg, Repr]) =
    case Lit(x) => 
      Right(ExpSym[Repr].lit(x))
    case Neg(t) => 
      recur(t).map(ExpSym[Repr].neg)
    case Add(l, r) => 
      for 
        l0 <- recur(l)
        r0 <- recur(r)
      yield ExpSym[Repr].add(l0, r0)
    case t => Left(s"Invalid tree: $t")

class MulSymInterpreter[Repr: MulSym] extends OpenInterpreter[Tree, Repr, ErrMsg]:
  def apply(recur: => Tree => Either[ErrMsg, Repr]) =
    case Mul(l, r) =>
      for
        l0 <- recur(l)
        r0 <- recur(r)
      yield MulSym[Repr].mul(l0, r0)
    case t => Left(s"Invalid tree: $t")

def parser[Repr: ExpSym: MulSym](t: Tree): Either[ErrMsg, Repr] = 
  (ExpSymInterpreter() orElse MulSymInterpreter() close) apply t
