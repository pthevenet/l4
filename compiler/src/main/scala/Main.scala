package main

import scala.io.Source

import l3._

object Main {
    def main(args: Array[String]): Unit = {
        val filename = "src/test/resources/parens.txt"
        val code = Source.fromFile(filename).getLines.mkString
        println(code)
        println(L3Lexer(code))
    }
}