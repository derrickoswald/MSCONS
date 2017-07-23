package ch.ninecode.edifact

import java.util.regex.Pattern

import scala.io.Source
import scala.util.parsing.combinator._

case class ServiceSegment (
    tag: String,
    name: String,
    description: String,
    fields: List[Field],
    notes: String)

case class Field (
    position: Int,
    tag: String,
    name: String,
    status: String,
    repetition: String,
    representation: String,
    notes: String,
    subfields: List[Field])

// parse Service segment specifications
// specification from http://www.gefeg.com/jswg/v4/data/v4.html
// link: http://www.gefeg.com/jswg/v4/data/s40000.zip
// edited and changed Ä to -
class ServiceSegmentList extends RegexParsers
{
    override val skipWhitespace = false

    // parse fixed field format:
    //Pos   TAG   Name                                        S R   Repr.    Notes
    //
    // of extracted text:
    //010   0085  SYNTAX ERROR, CODED                         M 1   an..3    
    //
    //020   S011  DATA ELEMENT IDENTIFICATION                 M 1            
    //      0098   Erroneous data element position in
    //             segment                                    M     n..3
    //      0104   Erroneous component data element
    //             position                                   C     n..3
    //      0136   Erroneous data element occurrence          C     n..6
    val content = new Parser[List[Field]]
    {
        def cut (string: String): Field =
        {
            val pos = string.substring (0, 3).trim
            val tag = string.substring (6, 12).trim
            val name = string.substring (12, Math.min (56, string.length)).trim
            val status = if (string.length > 56) string.substring (56, 57) else ""
            val rep = if (string.length > 58) string.substring (58, 62).trim else ""
            val repr = if (string.length > 62) string.substring (62, math.min (71, string.length)).trim else ""
            val notes = if (string.length > 71) string.substring (71, string.length) else ""
            Field (if ("" == pos) 0 else pos.toInt, tag, name, status, rep, repr, notes, List())
        }
        def parseField (string: String): Field =
        {
            val lines = string.split ("\n")
            val f = cut (lines(0))
            if (f.tag.startsWith ("S")) // composite
            {
                var line = 1
                var subs = List[Field]()
                while (line < lines.length)
                {
                    var s = cut (lines(line))
                    line += 1
                    if (s.status == "") // wrapped
                    {
                        val extra = cut (lines(line))
                        line += 1
                        s = s.copy (
                            name = s.name.trim + " " + extra.name.trim,
                            repetition = extra.repetition,
                            representation = extra.representation,
                            notes = s.notes.trim + extra.notes.trim)
                    }
                    subs = subs :+ s
                }
                f.copy (subfields = subs)
            }
            else
                if (1 < lines.length)
                    f.copy (notes = f.notes.trim + cut (lines(1)).notes.trim)
                else
                    f
        }
        def apply (in: Input) =
        {
            val source = in.source
            val offset = in.offset
            val parts = source.toString.split ("\n\n")
            val fields = parts.map (parseField).toList
            Success (fields, in.drop (in.toString.length))
        }
    }

    val servicesegment = new Parser[ServiceSegment]
    {
        val pattern = Pattern.compile ("""-{78}\n\n[\+\*\#\|X]?\s*(\S*)\s*(.*)\n\n\s*Function: ([\S\s]*?)\n\nPos   TAG   Name                                        S R   Repr.    Notes\n\n([\S\s]*?)(\n\n\n[\S\s]*?)??\n\n(?=-)""")
        def apply (in: Input) =
        {
            val source = in.source
            val offset = in.offset
            val sub = source.subSequence (offset, source.length)
            val matcher = pattern.matcher (sub)
            if (matcher.find ())
            {
//                println ("g1(" + matcher.group (1) + ")")
//                println ("g2(" + matcher.group (2) + ")")
//                println ("g3(" + matcher.group (3) + ")")
                var items: List[Field] = null
                val text = matcher.group (4)
                parse (content, text) match
                {
                    case Success (matched, _) => items = matched
                    case Failure (msg, _) => println ("FAILURE: " + msg)
                    case Error (msg, _) => println ("ERROR: " + msg)
                }
                val ss =
                    ServiceSegment (
                        matcher.group (1),
                        matcher.group (2),
                        matcher.group (3),
                        items,
                        matcher.group (5)
                       )
                Success (ss, in.drop (matcher.end))
            }
            else
                Failure ("servicesegment not found", in)
        }
    }

    def servicesegments: Parser[List[ServiceSegment]] = servicesegment.*
}
object TestServiceSegmentList extends ServiceSegmentList
{
    def main (args: Array[String]) =
    {
        val source = Source.fromFile ("ref/Ss40000.txt", "UTF-8")
        val text = source.getLines.mkString ("\n")
        source.close
        parse (servicesegments, text) match
        {
            case Success (matched, _) => println ("SUCCESS:\n" + matched.map (sc => sc.tag + " " + sc.name + " " + sc.fields.length + " fields").mkString ("\n"))
            case Failure (msg, _) => println ("FAILURE: " + msg)
            case Error (msg, _) => println ("ERROR: " + msg)
        }
    }
}