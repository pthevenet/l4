package l3

import scala.util.parsing.input.Positional

sealed trait L3Token extends Positional

case class OPAREN() extends L3Token
case class CPAREN() extends L3Token
case class COLON() extends L3Token
// case class INTEGER() extends L3Token
// case class IDENTIFIER() extends L3Token
