package l3

import l3._

import scala.util.parsing.combinator.RegexParsers

object L3Lexer extends RegexParsers {
    override def skipWhitespace = true
    override val whiteSpace = "[ \t\r\f\n]+".r

    def apply(code: String): Either[L3LexerError, Seq[L3Token]] = {
        parse(tokens, code) match {
            case Success(result, next) => Right(result)
            case NoSuccess(msg, next) => Left(L3LexerError(Location(next.pos.line, next.pos.column), msg))
        }
    }

    def tokens: Parser[Seq[L3Token]] = {
        phrase(rep(oparen | cparen))
    }

    def oparen: Parser[OPAREN] = {
        "\\(".r ^^ (_ => OPAREN())
    }

    def cparen: Parser[CPAREN] = {
        "\\)".r ^^ (_ => CPAREN())
    }

}