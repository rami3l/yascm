package io.github.rami3l.yascm.core

import cats.effect.IO
import cats.effect.Ref

type IORef[T] = Ref[IO, T]
