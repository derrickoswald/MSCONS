package ch.ninecode.edifact

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader

trait FieldExtractor extends Parsers
{
    val log: Logger = LoggerFactory.getLogger (getClass)
    type Elem = Field
    override type Input = Reader[Field]

    def alphanumeric (chars: Int): Parser[String] = acceptIf (x => x.text.length <= chars)(field => s"$field > $chars characters") ^^ (x => x.text)

    def fields[T,Q] (phrase: Parser[Q], constructor: Q => T): Parser[T] =
        Parser
        {
            in =>
                def parse (p: Parser[Q], in: FieldListParser): ParseResult[Q] = p(in)

                parse (phrase, FieldListParser (in.first.submembers)) match
                {
                    case Success (r, rest) =>
                        if (rest.atEnd)
                            Success (constructor (r), in.rest)
                        else
                        {
                            log.error ("too many subfields")
                            Error ("too many subfields", in.rest)
                        }
                    case Failure (msg, rest) =>
                        log.error (s"bad subfields $msg")
                        Error ("bad subfields", in.rest)
                    case Error (msg, rest) =>
                        log.error (s"bad subfields: $msg")
                        Error ("bad subfields", in.rest)
                }
        }

    def segments[T,Q] (phrase: Parser[Q], constructor: Q => T): Parser[T] =
        Parser
        {
            in =>
                def parse (p: Parser[Q], in: Input): ParseResult[Q] = p(in)

                parse (phrase, in) match
                {
                    case Success (r, rest) =>
                        if (rest.atEnd)
                            Success (constructor (r), rest)
                        else
                        {
                            log.error ("too many fields")
                            Error ("too many fields", rest)
                        }
                    case Failure (msg, rest) =>
                        log.error (s"bad fields $msg")
                        Error ("bad fields", rest)
                    case Error (msg, rest) =>
                        log.error (s"bad fields: $msg")
                        Error ("bad fields", rest)
                }
        }
}
