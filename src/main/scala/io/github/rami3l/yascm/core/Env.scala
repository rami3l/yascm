package io.github.rami3l.yascm.core

import collection.immutable.HashMap
import scala.compiletime.ops.boolean
import scala.annotation.tailrec
import cats.Alternative
import cats.effect.{IO, Ref}
import cats.syntax.all._
import cats.data.OptionT
import cats.implicits._
import monocle.syntax.all._

case class Env(
    val dict: HashMap[String, Exp] = HashMap(),
    val outer: Option[IORef[Env]]
)

extension (boxEnv: IORef[Env]) {
  private def lookupWithEnv(sym: String): OptionT[IO, (IORef[Env], Exp)] =
    OptionT.liftF(boxEnv.get).flatMap { env =>
      env.dict
        .get(sym)
        .map(defn => OptionT.some[IO](boxEnv -> defn))
        .getOrElse {
          OptionT
            .fromOption[IO](env.outer)
            .flatMap(o => o.lookupWithEnv(sym))
        }
    }

  /** Find the definition of a symbol.
    */
  def lookup(sym: String): OptionT[IO, Exp] = lookupWithEnv(sym).map(_._2)

  def insertVal(sym: String, defn: Exp): IO[Unit] =
    boxEnv.modify { env => env.focus(_.dict).modify(_ + (sym -> defn)) -> () }

  def setVal(sym: String, defn: Exp): IO[Unit] =
    lookupWithEnv(sym)
      .map(_._1)
      .getOrElse(boxEnv)
      .flatMap(_.insertVal(sym, defn))
}
