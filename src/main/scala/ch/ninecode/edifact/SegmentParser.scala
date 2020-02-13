package ch.ninecode.edifact

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader

case class SegmentParser (una: UNA) extends Parsers
{
    type Elem = String
    override type Input = Reader[String]
    implicit def segment: Parser[Segment] = new Parser[Segment]
    {
        override def apply (in: Input): ParseResult[Segment] =
        {
            if (in.atEnd)
                Failure ("EOF", in)
            else
            {
                val string = in.first
                val sep = string.indexOf (una.data_element_separator)
                val name = if (sep > 0)
                    string.substring (0, sep)
                else
                    string // see for example UIT INTERACTIVE MESSAGE TRAILER that has no mandatory fields
                if ((name.length == 3) && name.forall (_.isUpper))
                    {
                        val body = string.substring (sep + 1)
                        val seg = Segment (name, body)
                        Success (seg, in.rest)
                    }
                    else
                        Error (s"segment name not found (${name.substring (0, Math.min (20, name.length))})", in.rest)
            }
        }
    }
    def parse[T](p: Parser[T], in: SegmentScanner): ParseResult[T] = p(in)
}
