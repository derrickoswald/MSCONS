package ch.ninecode.edifact

import java.util.regex.Pattern

import scala.io.Source
import scala.util.parsing.combinator._

case class ServiceCode (
    number: Int,
    title: String,
    description: String,
    representation: String,
    items: List[Code])

case class Code (
    value: String,
    title: String,
    description: String)

// parse Service code list UNSL
// service code list from http://www.gefeg.com/jswg/v4x/data/v4x.html
// link: http://www.gefeg.com/jswg/cl/data/sl40210.zip
// edited and converted to UTF-8 encoding

class ServiceCodeList extends RegexParsers
{
    override val skipWhitespace = false
    def header: Parser[String]    = """[^-]*""".r ^^ { _.toString }
    val code = new Parser[Code]
    {
        val pattern = Pattern.compile ("""\s*(\S*)\s*(.*)\n((?:             [ \t\x0B\f\S]*\n)*)""")
        def apply (in: Input) =
        {
            val source = in.source
            val offset = in.offset
            val matcher = pattern.matcher (source.subSequence (offset, source.length))
            if (matcher.find ())
            {
//                println ("g1(" + matcher.group (1) + ")")
//                println ("g2(" + matcher.group (2) + ")")
//                println ("g3(" + matcher.group (3).substring (0, Math.min (20, matcher.group (3).length)) + "...)")
                def removeLeadingSpaces (s: String): String = s.split ("\n").map (_.trim).mkString ("\n")
                val cd =
                    Code (
                        matcher.group (1),
                        matcher.group (2),
                        removeLeadingSpaces (matcher.group (3))
                       )
                Success (cd, in.drop (matcher.end))
            }
            else
                Failure ("Code not found", in)
        }
    }
    val servicecode = new Parser[ServiceCode]
    {
        val pattern = Pattern.compile ("""-{70}\n\n[\+\*\#\|X]?\s*(\d*)\s*(.*)\n\n\s*?Desc: ([\S\s]*?)\n\n\s*Repr: (\S*)\n\n([\S\s]*?)\n\n""")
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
//                println ("g4(" + matcher.group (4) + ")")
                var items: List[Code] = null
                val text = matcher.group (5)
                parse (codes, text) match
                {
                    case Success (matched, _) => items = matched
                    case Failure (msg, _) => println ("FAILURE: " + msg)
                    case Error (msg, _) => println ("ERROR: " + msg)
                }
                val sc =
                    ServiceCode (
                        matcher.group (1).toInt,
                        matcher.group (2),
                        matcher.group (3),
                        matcher.group (4),
                        items
                       )
                Success (sc, in.drop (matcher.end))
            }
            else
                Failure ("ServiceCode not found", in)
        }
    }

    def codes: Parser[List[Code]] = code.*
    def servicecodes: Parser[List[ServiceCode]] = header ~ servicecode.* ^^ { case h ~ s => s }
}
object TestServiceCodeList extends ServiceCodeList
{
    def main (args: Array[String]) =
    {
        val source = Source.fromFile ("ref/sl40210.txt", "UTF-8")
        val text = source.getLines.mkString ("\n")
        source.close
        parse (servicecodes, text) match
        {
            case Success (matched, _) => println ("SUCCESS:\n" + matched.map (sc => sc.number + " " + sc.title + " (" + sc.representation + ")").mkString ("\n"))
                println (matched.filter (_.number == 65).head.items.filter (_.value == "MSCONS").head)
            case Failure (msg, _) => println ("FAILURE: " + msg)
            case Error (msg, _) => println ("ERROR: " + msg)
        }
    }
}