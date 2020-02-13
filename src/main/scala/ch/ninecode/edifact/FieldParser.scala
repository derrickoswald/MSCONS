package ch.ninecode.edifact

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader

case class FieldParser () extends Parsers
{
    type Elem = Field
    override type Input = Reader[Field]
    implicit def field: Parser[Field] = new Parser[Field]
    {
        override def apply (in: Input): ParseResult[Field] =
        {
            if (in.atEnd)
                Failure ("EOF", in)
            else
            {
                val field = in.first
                Success (field, in.rest)
            }
        }
    }
    def parse[T](p: Parser[T], in: FieldScanner): ParseResult[T] = p(in)
}