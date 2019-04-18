package l3

import org.scalatest.FunSuite
import scala.io.Source


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

}