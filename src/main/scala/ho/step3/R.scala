package ho
package step3

opaque type R[A] = A // basically the identity type

object R:
  def apply[A](a: A): R[A] = a

  extension on [A](ra: R[A]):
    def unR: A = ra
