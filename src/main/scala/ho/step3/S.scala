package ho
package step3

opaque type S[A] = Int => String
object S:
  def apply[A](f: Int => String): S[A] = f

  extension on [A](s: S[A]):
    def unS: Int => String = s
