package io.github.rami3l.yascm.core

import collection.mutable.HashMap
import scala.compiletime.ops.boolean
import scala.annotation.tailrec
import cats.effect.{IO, Ref}
import cats.syntax.all._

class Env(val dict: HashMap[String, Exp] = HashMap(), val outer: Option[Env]) {
  private def lookupWithEnv(sym: String): Option[(Env, Exp)] =
    dict.get(sym).map { (this, _) }.orElse {
      outer.flatMap(_.lookupWithEnv(sym))
    }

  /** Find the definition of a symbol.
    */
  def lookup(sym: String): Option[Exp] = lookupWithEnv(sym).map(_._2)

  def insertVal(sym: String, defn: Exp): Unit = dict += (sym -> defn)

  def setVal(sym: String, defn: Exp): Unit =
    lookupWithEnv(sym).map(_._1).getOrElse(this).insertVal(sym, defn)
}
