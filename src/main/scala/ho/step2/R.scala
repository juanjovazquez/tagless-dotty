package ho
package step2

opaque type R[Env, A] = Env => A
object R:
  def apply[Env, A](f: Env => A): R[Env, A] = f

  extension on [Env, A](r: R[Env, A]):
    def unR: Env => A = r