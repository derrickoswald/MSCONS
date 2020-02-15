package ch.ninecode.mscons

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader

import ch.ninecode.edifact.Segment

case class MSCONSMessage04B (
    bgm: BGM,
    dtm: DTM
)

object MSCONSMessage04B extends Parsers
{
    val log: Logger = LoggerFactory.getLogger (getClass)
    type Elem = Segment
    override type Input = Reader[Segment]

    def expect[Q] (name: String, constructor: Segment => Q): Parser[Q] =
        acceptIf (x => x.name == name)(segment => s"expected $name, got ${segment.name}") ^^
            (x => constructor (x))

    lazy val bgm: Parser[BGM] = expect ("BGM", x => BGM (x))
    lazy val dtm: Parser[DTM] = expect ("DTM", x => DTM (x))

    val phrase: Parser[MSCONSMessage04B] = bgm ~ dtm ^^ (x => MSCONSMessage04B (x._1, x._2))
}
