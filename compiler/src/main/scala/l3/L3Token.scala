package l3

import scala.util.parsing.input.Positional

sealed trait L3Token extends Positional

case class OPAREN() extends L3Token
case class CPAREN() extends L3Token
case class COLON() extends L3Token
case class IDENTIFIER(str: String) extends L3Token
case class DEF() extends L3Token
case class DEFREC() extends L3Token
case class FUN() extends L3Token
case class LET() extends L3Token
case class LETSTAR() extends L3Token
case class LETREC() extends L3Token
case class REC() extends L3Token
case class BEGIN() extends L3Token
case class IF() extends L3Token
case class COND() extends L3Token
case class AND() extends L3Token
case class OR() extends L3Token
case class NOT() extends L3Token
case class PRIMITIVE(name: String) extends L3Token
case class NUMBER(value: Int) extends L3Token
case class STRING(value: String) extends L3Token
case class CHARACTER(value: Char) extends L3Token
case class BOOLEAN(value: Boolean) extends L3Token
case class UNIT() extends L3Token
