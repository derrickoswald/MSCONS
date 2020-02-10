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
                if (string.length >= 3)
                {
                    val sep = string.indexOf (una.data_element_separator)
                    if (sep > 0)
                    {
                        val name = string.substring (0, sep)
                        if (name.forall (_.isUpper))
                        {
                            val seg = Segment (name, string.substring (sep + 1))
                            Success (seg, in.rest)
                        }
                        else
                            Failure ("name is not all upper case", in.rest)
                    }
                    else
                        Error ("data element separator not found", in.rest)
                }
                else
                    Error ("segment name not found", in.rest)
            }
        }
    }
    def parse[T](p: Parser[T], in: SegmentScanner): ParseResult[T] = p(in)
}
