package ch.ninecode.edifact

import java.nio.ByteBuffer
import java.util.regex.Pattern

import scala.collection.immutable.PagedSeq
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{PagedSeqReader, Reader}

case class ByteBufferIterator (buffer: ByteBuffer) extends Iterator[Char]
{
    override def hasNext: Boolean = buffer.hasRemaining

    override def next (): Char = buffer.get.asInstanceOf[Char]
}

class ByteBufferReader (buffer: ByteBuffer, override val offset: Int) extends PagedSeqReader (PagedSeq.fromIterator (ByteBufferIterator (buffer)), offset)
{
    /** Construct a `ByteBufferReader` with its first element at
     *  `source(0)` and position `(1,1)`.
     */
    def this (buffer: ByteBuffer) = this (buffer, 0)
}

case class Segment (name: String, contents: List[String])
{

}

class SegmentParser extends Parsers
{
    type Elem = Char

    /** Parse some prefix of reader `in` with parser `p`. */
    def parse[T] (p: Parser[T], in: Reader[Char]): ParseResult[T] =
        p(in)

    val pattern: Pattern = Pattern.compile ("""UNA(.)(.)(.)(.)(.)(.)""")
    val unaparser = new Parser[UNA]
    {
        def apply (in: Input): ParseResult[UNA] =
        {
            val source = in.source
            val offset = in.offset
            val matcher = pattern.matcher (source.subSequence (offset, offset + 9)) // at most 9 characters
            if (matcher.find ())
                Success (
                    UNA (
                        matcher.group (0).codePointAt (0),
                        matcher.group (1).codePointAt (0),
                        matcher.group (2).codePointAt (0),
                        matcher.group (4).codePointAt (0),
                        matcher.group (6).codePointAt (0)
                    ),
                    in.drop (matcher.end - offset)
                )
            else
                Failure ("", in)
        }
    }

    def segment (una: UNA) = new Parser[Segment]
    {
        val term: Int = una.segment_terminator
        val release: Int = una.release_character
        def descend (i: Int, in: Input): ParseResult[Array[Char]] =
        {
            if (in.atEnd)
                Error ("segment terminator not found", in)
            else
            {
                val c = in.first
                if (term == c)
                    Success (new Array [Char](i), in.rest)
                else if (release == c)
                {
                    val rest = in.rest
                    if (rest.atEnd)
                        Error ("release character at end of input", in)
                    else
                    {
                        val c2 = rest.first
                        val ret = descend (i + 1, rest.rest)
                        ret match
                        {
                            case Success (array: Array[Char], _) => array(i) = c2
                            case _ =>
                        }
                        ret
                    }
                }
                else
                {
                    val ret = descend (i + 1, in.rest)
                    ret match
                    {
                        case Success (array: Array[Char], _) => array(i) = c
                        case _ =>
                    }
                    ret
                }
            }
        }

        def apply (in: Input): ParseResult[Segment] =
        {
            if (in.atEnd)
                Failure ("end of input", in)
            else
            {
                val result = descend (0, in)
                result match
                {
                    case Success (array: Array[Char], rest: Input) =>
                        if (array.length >= 3)
                        {
                            val name = array.subSequence (0, 3).toString
                            if (name.forall (_.isUpper))
                            {
                                val seg =
                                    Segment (
                                        name,
                                        List [String](array.subSequence (3, array.length).toString)
                                    )
                                Success (seg, rest)
                            }
                            else
                                Failure ("name is not all upper case", rest)
                        }
                        else
                            Error ("segment name not found", rest)
                    case Failure (msg, rest) => Failure (msg, rest)
                    case Error (msg, rest) => Error (msg, rest)
                }
            }
        }
    }

    def message: Parser[List[Segment]] =
    {
        unaparser | success (UNA ()) into (una => segment (una).*)
    }
}
