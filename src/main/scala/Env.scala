package com.github.rami3l.yascm

import collection.mutable.HashMap

class Env(val dict: HashMap[String, Exp], val outer: Option[Env]) {
  def this(outer: Env) = {
    this(new HashMap(), Some(outer))
  }

  /** Find the definition of a symbol.
    */
  def lookup(sym: String): Option[Exp] = {
    this.dict.get(sym).orElse {
      this.outer.flatMap { _.lookup(sym) }
    }
  }

  def insertVal(sym: String, defn: Exp): Unit = {
    this.dict += (sym -> defn)
  }

  def setVal(sym: String, defn: Exp): Unit = {
    lazy val isSymLocal = this.dict.get(sym).isDefined
    lazy val isSymDefined = this.lookup(sym).isDefined
    if (!isSymLocal && isSymDefined) {
      // If `sym` is not local but is defined, then `outer` must be defined.
      this.outer.get.setVal(sym, defn)
    } else {
      this.insertVal(sym, defn)
    }
  }

}
