package io.github.rami3l.yascm

import collection.mutable.HashMap
import scala.compiletime.ops.boolean
import scala.annotation.tailrec

class Env(val dict: HashMap[String, Exp] = HashMap(), val outer: Option[Env]) {

  /** Find the definition of a symbol.
    */
  def lookup(sym: String): Option[Exp] =
    dict.get(sym).orElse { outer.flatMap { _.lookup(sym) } }

  def insertVal(sym: String, defn: Exp): Unit = dict += (sym -> defn)

  @tailrec
  final def setVal(
      sym: String,
      defn: Exp,
      isSymDefined: Boolean = false
  ): Unit = {
    lazy val isSymLocal = dict.get(sym).isDefined
    lazy val isSymDefined1 = isSymDefined || lookup(sym).isDefined
    if (!isSymLocal && isSymDefined1)
      // If `sym` is not local but is defined, then `outer` must be defined.
      outer.get.setVal(sym, defn, true)
    else
      insertVal(sym, defn)
  }
}
