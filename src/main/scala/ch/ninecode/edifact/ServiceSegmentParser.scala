package ch.ninecode.edifact

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader

object ServiceSegmentParser extends Parsers
{
    val log: Logger = LoggerFactory.getLogger (getClass)
    type Elem = Segment
    override type Input = Reader[Segment]

    def unb: Parser[UNB] =
    {
        Parser
        {
            in =>
                if (in.atEnd) Failure ("end of input", in)
                else if (in.first.name == "UNB") Success (UNB (in.first.fields), in.rest)
                else Failure (in.first.name, in)
        }
    }

    def unh: Parser[UNH] =
    {
        Parser
        {
            in =>
                if (in.atEnd) Failure ("end of input", in)
                else if (in.first.name == "UNH") Success (UNH (in.first.fields), in.rest)
                else Failure (in.first.name, in)
        }
    }

    val header = unb ~ unh
    def parse[T](p: Parser[T], in: SegmentListParser): ParseResult[T] = p(in)

    def read[ServiceSegments](in: List[Segment]): ParseResult[ch.ninecode.edifact.ServiceSegments] =
    {
        val x = parse (header, SegmentListParser (in))
        x match
        {
            case Success (r, rest) =>
                Success (ServiceSegments (r._1, r._2), rest)
            case Failure (msg, rest) =>
                Failure (s"service segments parse failure: $msg", rest)
            case Error (msg, rest) =>
                Error (s"service segments parse error: $msg", rest)
        }
    }
}
