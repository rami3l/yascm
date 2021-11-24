package io.github.rami3l.yascm

import scala.util.{Success, Failure, Try}
import cats.arrow.FunctionK
import cats.data.State

// Shamelessly copied from https://earldouglas.com/posts/itof/var-to-state.html#example-stateful-references
class STRef[S, A](_a: A) {
  private var a: A = _a

  def read: State[S, A] = State((_, a))

  def modify(f: A => A): State[S, Unit] = State { a = f(a); (_, ()) }

  def write(a1: A): State[S, Unit] = modify(_ => a1)
}

object STRef {
  def newState[S, A](a: A): State[S, STRef[S, A]] = State((_, new STRef(a)))
}

extension [T](o: Option[T]) {
  def successOr(e: => Throwable): Try[T] =
    o.map(Success.apply).getOrElse(Failure(e))
}

object OptionExt {
  def successOrK(e: => Throwable) = new FunctionK[Option, Try] {
    def apply[T](o: Option[T]): Try[T] = o.successOr(e)
  }
}
