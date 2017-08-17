package ch.ninecode.edifact

import java.nio.ByteBuffer

import scala.io.Source
import org.scalatest.FunSuite

import scala.util.parsing.input.Reader

class EDIFACTSuite extends FunSuite
{
    test ("ParseServiceSegmentList")
    {
        val source = Source.fromFile ("ref/Ss40000.txt", "UTF-8")
        val text = source.getLines.mkString ("\n")
        source.close
        val parser = new ParseServiceSegmentList ()
        parser.parse (parser.list, text) match
        {
            case parser.Success (matched: List[ServiceSegment], _) =>
                // println ("SUCCESS:\n" + matched.map (sc => sc.tag + " " + sc.name + " " + sc.fields.length + " fields").mkString ("\n"))
                // println (matched.filter (_.tag == "UNB").head)
                assert (matched.length == 34)
                //first segment
                assert (matched.head.tag == "UCD")
                assert (matched.head.name == "DATA ELEMENT ERROR INDICATION")
                assert (matched.head.description == "To identify an erroneous stand-alone, composite or component\ndata element, and to identify the nature of the error.")
                assert (matched.head.notes == null)
                assert (matched.head.fields.length == 2)
                // first field
                assert (matched.head.fields.head.position == 10)
                assert (matched.head.fields.head.tag == "0085")
                assert (matched.head.fields.head.name == "SYNTAX ERROR, CODED")
                assert (matched.head.fields.head.status == "M")
                assert (matched.head.fields.head.repetition == "1")
                assert (matched.head.fields.head.representation == "an..3")
                assert (matched.head.fields.head.notes == "")
                assert (matched.head.fields.head.subfields.isEmpty)
                // last field
                assert (matched.head.fields.last.position == 20)
                assert (matched.head.fields.last.tag == "S011")
                assert (matched.head.fields.last.name == "DATA ELEMENT IDENTIFICATION")
                assert (matched.head.fields.last.status == "M")
                assert (matched.head.fields.last.repetition == "1")
                assert (matched.head.fields.last.representation == "")
                assert (matched.head.fields.last.notes == "")
                assert (matched.head.fields.last.subfields.length == 3)
                // first subfield
                assert (matched.head.fields.last.subfields.head.position == 0)
                assert (matched.head.fields.last.subfields.head.tag == "0098")
                assert (matched.head.fields.last.subfields.head.name == "Erroneous data element position in segment")
                assert (matched.head.fields.last.subfields.head.status == "M")
                assert (matched.head.fields.last.subfields.head.repetition == "")
                assert (matched.head.fields.last.subfields.head.representation == "n..3")
                assert (matched.head.fields.last.subfields.head.notes == "")
                assert (matched.head.fields.last.subfields.head.subfields.isEmpty)
                // last subfield
                assert (matched.head.fields.last.subfields.last.position == 0)
                assert (matched.head.fields.last.subfields.last.tag == "0136")
                assert (matched.head.fields.last.subfields.last.name == "Erroneous data element occurrence")
                assert (matched.head.fields.last.subfields.last.status == "C")
                assert (matched.head.fields.last.subfields.last.repetition == "")
                assert (matched.head.fields.last.subfields.last.representation == "n..6")
                assert (matched.head.fields.last.subfields.last.notes == "")
                assert (matched.head.fields.last.subfields.last.subfields.isEmpty)
            case parser.Failure (msg, _) => fail (msg)
            case parser.Error (msg, _) => fail (msg)
        }
    }

    test ("ParseServiceCodeList")
    {
        val source = Source.fromFile ("ref/sl40210.txt", "UTF-8")
        val text = source.getLines.mkString ("\n")
        source.close
        val parser = new ParseServiceCodeList ()
        parser.parse (parser.list, text) match
        {
            case parser.Success (matched: List[CodeList], _) =>
                // println ("SUCCESS:\n" + matched.map (sc => sc.number + " " + sc.title + " (" + sc.representation + ")").mkString ("\n"))
                // println (matched.filter (_.number == 65).head.items.filter (_.value == "MSCONS").head)
                assert (matched.length == 52)
                //first
                assert (matched.head.number == 1)
                assert (matched.head.title == "Syntax identifier")
                assert (matched.head.description == "Coded identification of the agency controlling the syntax, and\nof the character repertoire used in an interchange.")
                assert (matched.head.representation == "a4")
                assert (matched.head.items.length == 15)
                assert (matched.head.items.head.value == "UNOA")
                assert (matched.head.items.head.title == "UN/ECE level A")
                assert (matched.head.items.head.description == "As defined in the basic code table of ISO 646 with the\nexceptions of lower case letters, alternative graphic\ncharacter allocations and national or application-\noriented graphic character allocations.")
                assert (matched.head.items.last.value == "UNOY")
                assert (matched.head.items.last.title == "UN/ECE level Y")
                assert (matched.head.items.last.description == "ISO 10646-1 octet without code extension technique.")

            case parser.Failure (msg, _) => fail (msg)
            case parser.Error (msg, _) => fail (msg)
        }
    }

    test ("ParseCodeList")
    {
        val source = Source.fromFile ("ref/d17a/uncl/UNCL.17A", "UTF-8")
        val text = source.getLines.mkString ("\n")
        source.close
        val parser = new ParseCodeList ()
        parser.parse (parser.list, text) match
        {
            case parser.Success (matched: List[CodeList], _) =>
                // println ("SUCCESS:\n" + matched.map (sc => sc.number + " " + sc.title + " (" + sc.representation + ")").mkString ("\n"))
                assert (matched.length == 272)
                //first
                assert (matched.head.number == 1001)
                assert (matched.head.title == "Document name code                                      [C]")
                assert (matched.head.description == "Code specifying the document name.")
                assert (matched.head.representation == "an..3")
                assert (matched.head.items.length == 727)
                assert (matched.head.items.head.value == "1")
                assert (matched.head.items.head.title == "Certificate of analysis")
                assert (matched.head.items.head.description == "Certificate providing the values of an analysis.")
                assert (matched.head.items.last.value == "998")
                assert (matched.head.items.last.title == "Previous Customs document/message")
                assert (matched.head.items.last.description == "Indication of the previous Customs document/message\nconcerning the same transaction.")
                // last
                assert (matched.last.number == 9649)
                assert (matched.last.title == "Processing information code qualifier                   [B]")
                assert (matched.last.description == "Code qualifying the processing information.")
                assert (matched.last.representation == "an..3")
                assert (matched.last.items.length == 6)
                assert (matched.last.items.head.value == "1")
                assert (matched.last.items.head.title == "Entity asset reporting")
                assert (matched.last.items.head.description == "To convey information related to the reporting of assets\nheld by an entity.")
                assert (matched.last.items.last.value == "6")
                assert (matched.last.items.last.title == "Statistical array processing")
                assert (matched.last.items.last.description == "Defines information required to process the contents of\na statistical array.")
            case parser.Failure (msg, _) => fail (msg)
            case parser.Error (msg, _) => fail (msg)
        }
    }

    test ("ParseSegmentwithUNA")
    {
        val buffer = ByteBuffer.wrap ("UNA:+.? 'UNB'".getBytes)
        val parser = new SegmentParser
        val reader: Reader[Char] = new ByteBufferReader (buffer)
        parser.parse (parser.message, reader) match
        {
            case parser.Success (matched: List[Segment], _) => assertResult ("UNB", "name incorrect") (matched.head.name)
            case parser.Failure (msg, _) => fail (msg)
            case parser.Error (msg, _) => fail (msg)
        }
    }

    test ("ParseSegmentwithoutUNA")
    {
        val buffer = ByteBuffer.wrap ("UNB'".getBytes)
        val parser = new SegmentParser
        val reader: Reader[Char] = new ByteBufferReader (buffer)
        parser.parse (parser.message, reader) match
        {
            case parser.Success (matched: List[Segment], _) => assertResult ("UNB", "name incorrect") (matched.head.name)
            case parser.Failure (msg, _) => fail (msg)
            case parser.Error (msg, _) => fail (msg)
        }
    }

    test ("ParseSegmentwithoutSegmentTerminator")
    {
        val buffer = ByteBuffer.wrap ("UNB".getBytes)
        val parser = new SegmentParser
        val reader: Reader[Char] = new ByteBufferReader (buffer)
        parser.parse (parser.message, reader) match
        {
            case parser.Success (matched: List[Segment], _) => fail ("shouldn't succeed")
            case parser.Failure (msg, _) => fail (msg)
            case parser.Error (msg, _) => assertResult ("segment terminator not found", "EOF not handled")(msg)
        }
    }
}

