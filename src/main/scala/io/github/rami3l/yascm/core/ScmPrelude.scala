package io.github.rami3l.yascm.core

import scala.util.Try
import collection.mutable.HashMap

object ScmPrelude {
  def env = Env(
    outer = None,
    dict = HashMap(
      "+" -> add,
      "-" -> sub,
      "*" -> mul,
      "/" -> div,
      "=" -> eq,
      "<" -> lt,
      "<=" -> le,
      ">" -> gt,
      ">=" -> ge,
      "car" -> car,
      "cdr" -> cdr,
      "cons" -> cons,
      "list" -> list,
      "nil?" -> isNil,
      "boolean?" -> isBoolean
    ).view.mapValues(Primitive.apply).to(HashMap)
  )

  def add2(x: Exp, y: Exp): Try[Exp] = Try {
    (x, y) match {
      case (ScmInt(x), ScmInt(y))       => ScmInt(x + y)
      case (ScmInt(x), ScmDouble(y))    => ScmDouble(x + y)
      case (ScmDouble(x), ScmInt(y))    => ScmDouble(x + y)
      case (ScmDouble(x), ScmDouble(y)) => ScmDouble(x + y)
      case _ => throw Exception("add: expected numbers")
    }
  }

  def add(xs: Seq[Exp]): Try[Exp] = Try {
    xs.fold(ScmInt(0))(add2(_, _).get)
  }

  def mul2(x: Exp, y: Exp): Try[Exp] = Try {
    (x, y) match {
      case (ScmInt(x), ScmInt(y))       => ScmInt(x * y)
      case (ScmInt(x), ScmDouble(y))    => ScmDouble(x * y)
      case (ScmDouble(x), ScmInt(y))    => ScmDouble(x * y)
      case (ScmDouble(x), ScmDouble(y)) => ScmDouble(x * y)
      case _ => throw Exception("mul: expected numbers")
    }
  }

  def mul(xs: Seq[Exp]): Try[Exp] = Try {
    xs.fold(ScmInt(1))(mul2(_, _).get)
  }

  def sub(xs: Seq[Exp]): Try[Exp] = Try {
    xs match {
      case Seq(ScmInt(x), ScmInt(y))       => ScmInt(x - y)
      case Seq(ScmInt(x), ScmDouble(y))    => ScmDouble(x - y)
      case Seq(ScmDouble(x), ScmInt(y))    => ScmDouble(x - y)
      case Seq(ScmDouble(x), ScmDouble(y)) => ScmDouble(x - y)
      case _ => throw Exception("sub: expected numbers")
    }
  }

  def div(xs: Seq[Exp]): Try[Exp] = Try {
    xs match {
      case Seq(ScmInt(x), ScmInt(y))       => ScmDouble(x.toDouble / y)
      case Seq(ScmInt(x), ScmDouble(y))    => ScmDouble(x / y)
      case Seq(ScmDouble(x), ScmInt(y))    => ScmDouble(x / y)
      case Seq(ScmDouble(x), ScmDouble(y)) => ScmDouble(x / y)
      case _ => throw Exception("div: expected numbers")
    }
  }

  def eq(xs: Seq[Exp]): Try[Exp] = Try {
    xs match {
      case Seq(x, y) => ScmBool(x == y)
      case _         => throw Exception("eq: expected 2 expressions")
    }
  }

  def lt(xs: Seq[Exp]): Try[Exp] = Try {
    xs match {
      case Seq(ScmInt(x), ScmInt(y))       => ScmBool(x < y)
      case Seq(ScmInt(x), ScmDouble(y))    => ScmBool(x < y)
      case Seq(ScmDouble(x), ScmInt(y))    => ScmBool(x < y)
      case Seq(ScmDouble(x), ScmDouble(y)) => ScmBool(x < y)
      case _ => throw Exception("lt: expected numbers")
    }
  }

  def le(xs: Seq[Exp]): Try[Exp] = Try {
    xs match {
      case Seq(ScmInt(x), ScmInt(y))       => ScmBool(x <= y)
      case Seq(ScmInt(x), ScmDouble(y))    => ScmBool(x <= y)
      case Seq(ScmDouble(x), ScmInt(y))    => ScmBool(x <= y)
      case Seq(ScmDouble(x), ScmDouble(y)) => ScmBool(x <= y)
      case _ => throw Exception("le: expected numbers")
    }
  }

  def gt(xs: Seq[Exp]): Try[Exp] = Try {
    xs match {
      case Seq(ScmInt(x), ScmInt(y))       => ScmBool(x > y)
      case Seq(ScmInt(x), ScmDouble(y))    => ScmBool(x > y)
      case Seq(ScmDouble(x), ScmInt(y))    => ScmBool(x > y)
      case Seq(ScmDouble(x), ScmDouble(y)) => ScmBool(x > y)
      case _ => throw Exception("gt: expected numbers")
    }
  }

  def ge(xs: Seq[Exp]): Try[Exp] = Try {
    xs match {
      case Seq(ScmInt(x), ScmInt(y))       => ScmBool(x >= y)
      case Seq(ScmInt(x), ScmDouble(y))    => ScmBool(x >= y)
      case Seq(ScmDouble(x), ScmInt(y))    => ScmBool(x >= y)
      case Seq(ScmDouble(x), ScmDouble(y)) => ScmBool(x >= y)
      case _ => throw Exception("ge: expected numbers")
    }
  }

  def car(xs: Seq[Exp]): Try[Exp] = Try {
    xs match {
      case Seq(Cons(car, _)) => car
      case _                 => throw Exception("car: expected a cons")
    }
  }

  def cdr(xs: Seq[Exp]): Try[Exp] = Try {
    xs match {
      case Seq(Cons(_, cdr)) => cdr
      case _                 => throw Exception("cdr: expected a cons")
    }
  }

  def cons(xs: Seq[Exp]): Try[Exp] = Try {
    xs match {
      case Seq(x, y) => Cons(car = x, cdr = y)
      case _         => throw Exception("cons: expected 2 expressions")
    }
  }

  def list(xs: Seq[Exp]): Try[Exp] = Try(ConsCell.fromSeq(xs))

  def isNil(xs: Seq[Exp]): Try[Exp] = Try {
    xs match {
      case Seq(ScmNil) => ScmBool(true)
      case _           => ScmBool(false)
    }
  }

  def isBoolean(xs: Seq[Exp]): Try[Exp] = Try {
    xs match {
      case Seq(ScmBool(_)) => ScmBool(true)
      case _               => ScmBool(false)
    }
  }
}
