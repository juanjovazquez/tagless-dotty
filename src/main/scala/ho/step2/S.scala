package ho
package step2

opaque type S[Env, A] = Int => String
object S:
  def apply[Env, A](f: Int => String): S[Env, A] = f

  extension on [Env, A](s: S[Env, A]):
    def unS: Int => String = s