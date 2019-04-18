package l3

sealed trait L3CompilationError

case class L3LexerError(location: Location, msg: String) extends L3CompilationError
case class L3ParserError(location: Location, msg: String) extends L3CompilationError

case class Location(line: Int, column: Int) {
    override def toString = s"$line:$column"
}