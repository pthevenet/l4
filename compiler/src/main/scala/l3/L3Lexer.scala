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

    private def identstart = "a-zA-Z|!%&*+-.\\/:<=>?^_~"
    private def digit = "0-9"
    private def blockPrimitives = "block-alloc-[0-9]+|block\\?|block-tag|block-length|block-get|block-set!"
    private def intPrimitives = "int\\?|\\+|-|\\*|/|%|shift-left|shift-right|and|or|xor|<|[<][=]|byte-read|byte-write|int->char"
    private def charPrimitives = "char\\?|char->int"
    private def boolPrimitives = "bool\\?"
    private def unitPrimitives = "unit\\?"
    private def valuePrimitives = "=|id"
    private def primitives = s"$blockPrimitives|$intPrimitives|$charPrimitives|$boolPrimitives|$unitPrimitives|$valuePrimitives"
    
    def tokens: Parser[Seq[L3Token]] =
        phrase(rep(parseOParen | parseCParen | parseColon | // punctuations
            parseDefRec | parseLetRec | parseLetStar | // composite words
            parseDef | parseFun | parseLet | parseRec | parseBegin | // simple words
            parseIf | parseCond | parseAnd | parseOr | parseNot | // simple words
            parsePrimitive | 
            parseNumber | parseString | parseCharacter | parseBoolean | parseUnit |
            parseIdentifier))

    def parseOParen: Parser[OPAREN] =
        "\\(".r ^^ (_ => OPAREN())

    def parseCParen: Parser[CPAREN] =
        "\\)".r ^^ (_ => CPAREN())

    def parseColon: Parser[COLON] =
        ",".r ^^ (_ => COLON())

    def parseIdentifier: Parser[IDENTIFIER] =
        s"[$identstart][$identstart$digit]*[@]?[$digit]*".r ^^ (str => IDENTIFIER(str))

    def parseDef: Parser[DEF] =
        "def".r ^^ (_ => DEF())
    
    def parseDefRec: Parser[DEFREC] =
        "defrec".r ^^ (_ => DEFREC())

    def parseFun: Parser[FUN] =
        "fun".r ^^ (_ => FUN())

    def parseLet: Parser[LET] =
        "let".r ^^ (_ => LET())
    
    def parseLetStar: Parser[LETSTAR] =
        "let\\*".r ^^ (_ => LETSTAR())
    
    def parseLetRec: Parser[LETREC] =
        "letrec".r ^^ (_ => LETREC())

    def parseRec: Parser[REC] =
        "rec".r ^^ (_ => REC())

    def parseBegin: Parser[BEGIN] =
        "begin".r ^^ (_ => BEGIN())
    
    def parseIf: Parser[IF] =
        "if".r ^^ (_ => IF())
    
    def parseCond: Parser[COND] =
        "cond".r ^^ (_ => COND())

    def parseAnd: Parser[AND] =
        "and".r ^^ (_ => AND())

    def parseOr: Parser[OR] =
        "or".r ^^ (_ => OR())

    def parseNot: Parser[NOT] =
        "not".r ^^ (_ => NOT())

    def parsePrimitive: Parser[PRIMITIVE] =
        s"@($primitives)".r ^^ (str => PRIMITIVE(str.substring(1)))
    
    def parseNumber: Parser[NUMBER] =
        "[0-9]+".r ^^ (str => NUMBER(str.toInt))

    def parseString: Parser[STRING] =
        """"[^"]*"""".r ^^ (str => STRING(str.substring(1, str.length - 1)))

    def parseCharacter: Parser[CHARACTER] =
        "'[^']'".r ^^ (str => CHARACTER(str charAt 1))
    
    def parseBoolean: Parser[BOOLEAN] =
        "#[f|t]".r ^^ (str => if (str == "#f") BOOLEAN(false) else BOOLEAN(true))
    
    def parseUnit: Parser[UNIT] =
        "#u".r ^^ (_ => UNIT())
}