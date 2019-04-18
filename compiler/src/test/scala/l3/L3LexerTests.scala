package l3

import org.scalatest.FunSuite
import scala.io.Source
import org.scalatest.prop.TableDrivenPropertyChecks._

class L3LexerTest extends FunSuite {

    test("Invoking L3Lexer on sequence of parentheses should return an ordered sequence of parenthese tokens") {
        val code = "()(()))))((("
        val expected = Right(Seq(OPAREN(), CPAREN(), OPAREN(), OPAREN(), CPAREN(), CPAREN(), CPAREN(), CPAREN(), CPAREN(), OPAREN(), OPAREN(), OPAREN()))
        assertResult(expected)(L3Lexer(code))
    }

    test("Invoking L3Lexer on empty string should return an empty sequence of tokens") {
        val code = ""
        val expected = Right(Nil)
        assertResult(expected)(L3Lexer(code))
    }

    val singleTokens = Table (
        ("code", "expectedToken"),
        // keywords
        ("def", DEF()),
        ("defrec", DEFREC()),
        ("fun", FUN()),
        ("let", LET()),
        ("let*", LETSTAR()),
        ("letrec", LETREC()),
        ("rec", REC()),
        ("begin", BEGIN()),
        ("if", IF()),
        ("cond", COND()),
        ("and", AND()),
        ("or", OR()),
        ("not", NOT()),
        ("@=", PRIMITIVE("=")),
        ("ident@11", IDENTIFIER("ident@11")),
        ("ident@1", IDENTIFIER("ident@1")),
        ("I", IDENTIFIER("I")),
        ("I2", IDENTIFIER("I2")),
        ("~", IDENTIFIER("~")),
        ("~~~", IDENTIFIER("~~~")),
        ("~|1", IDENTIFIER("~|1")),
        ("~|1|1|@1", IDENTIFIER("~|1|1|@1"))
    )

    forAll (singleTokens) { (code: String, expectedToken: L3Token) => 
        assertResult(Right(Seq(expectedToken)))(L3Lexer(code))
    }

    test("Invoking L3Lexer on a small piece of code") {
        val code = "(let ((f 3)) #u)"
        val expected = Right(Seq(
            OPAREN(), LET(), OPAREN(), OPAREN(), IDENTIFIER("f"), NUMBER(3), CPAREN(), CPAREN(), UNIT(), CPAREN()
        ))
        assertResult(expected)(L3Lexer(code))
    }

}