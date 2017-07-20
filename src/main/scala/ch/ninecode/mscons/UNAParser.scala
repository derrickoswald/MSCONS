package ch.ninecode.mscons

import java.util.regex.Pattern

import scala.util.parsing.combinator._
import scala.util.parsing.input._

case class UNA (
    component_data_element_separator:Int = ":".codePointAt (0),
    data_element_separator:Int = "+".codePointAt (0),
    decimal_notification:Int = ".".codePointAt (0),
    release_character:Int = "?".codePointAt (0), // ToDo: a space character means the release character is not used
    segment_terminator:Int = "'".codePointAt (0)
)

class UNAParser extends RegexParsers
{
    //     component data element separator (:)
    //     data element separator (+)
    //     decimal notification (.)
    //     release character (?)
    //     reserved, must be a space
    //     segment terminator (')
    val pattern = Pattern.compile ("""UNA(.)(.)(.)(.)(.)(.)""")
    val una = new Parser[UNA]
    {
        def apply (in: Input) =
        {
            val source = in.source
            val offset = in.offset
            val matcher = pattern.matcher (source) // ToDo: use offset
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
                Success (UNA (), in)
        }
    }
    def parse (in: java.lang.CharSequence): ParseResult[UNA] =
        una (new CharSequenceReader (in))
}
