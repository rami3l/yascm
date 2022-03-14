package io.github.rami3l.yascm.core

import cats.effect.{IO, Ref}

type IORef[T] = Ref[IO, T]
