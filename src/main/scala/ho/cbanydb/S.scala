package ho.cbanydb

import cats.Monad
import cats.effect.{ LiftIO, Sync }

type S1[L, M[_]] = [x] =>> S[L, M, x]

opaque type S[L, M[_], A] = M[A]

object S:
  def apply[L] = SBuilder[L]

  class SBuilder[L]:
    def apply[M[_], A](ma: M[A]): S[L, M, A] = ma

  def unS[L, M[_], A](s: S[L, M, A]): M[A] = s

  given SIsMonad[L, M[_]](using M: Monad[M]) as Monad[S1[L, M]]    = M
  given SIsLiftIO[L, M[_]](using M: LiftIO[M]) as LiftIO[S1[L, M]] = M
  given SIsSync[L, M[_]](using M: Sync[M]) as Sync[S1[L, M]]       = M
