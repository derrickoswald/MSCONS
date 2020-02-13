package ch.ninecode.edifact

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.FileSystems
import java.nio.file.StandardOpenOption

import scala.io.Source
import org.scalatest.FunSuite

import scala.util.parsing.input.PagedSeq
import scala.util.parsing.input.PagedSeqReader
import scala.util.parsing.input.Reader

class EDIFACTSuite extends FunSuite
{
    test ("UNA 1 - default")
    {
        val scanner = SegmentScanner ("")
        assert (scanner.una.component_data_element_separator == ':')
        assert (scanner.una.data_element_separator == '+')
        assert (scanner.una.decimal_notification == '.')
        assert (scanner.una.release_character == '?')
        assert (scanner.una.segment_terminator == '\'')
    }

    test ("UNA 2 - read default")
    {
        val scanner = SegmentScanner ("UNA:+.? 'XYZ")
        assert (scanner.una.component_data_element_separator == ':')
        assert (scanner.una.data_element_separator == '+')
        assert (scanner.una.decimal_notification == '.')
        assert (scanner.una.release_character == '?')
        assert (scanner.una.segment_terminator == '\'')
        assert (!scanner.atEnd)
    }

    test ("UNA 3 - read non-default")
    {
        val scanner = SegmentScanner ("UNA:+,? 'XYZ")
        assert (scanner.una.component_data_element_separator == ':')
        assert (scanner.una.data_element_separator == '+')
        assert (scanner.una.decimal_notification == ',')
        assert (scanner.una.release_character == '?')
        assert (scanner.una.segment_terminator == '\'')
        assert (!scanner.atEnd)
    }

    test ("UNA 4 - terminator")
    {
        val scanner = SegmentScanner ("UNA:+.? ;XYZ;0")
        assert (scanner.una.component_data_element_separator == ':')
        assert (scanner.una.data_element_separator == '+')
        assert (scanner.una.decimal_notification == '.')
        assert (scanner.una.release_character == '?')
        assert (scanner.una.segment_terminator == ';')
        val seg = scanner.first
        assertResult ("XYZ", "segment incorrect")(seg)
        assert (!scanner.atEnd)
    }

    test ("UNA 5 - none")
    {
        val scanner = SegmentScanner ("UNB'")
        val unb = scanner.first
        assertResult ("UNB", "segment incorrect")(unb)
    }

    test ("UNA 6 - none no terminator")
    {
        val scanner = SegmentScanner ("XYZ;")
        assert (scanner.una.component_data_element_separator == ':')
        assert (scanner.una.data_element_separator == '+')
        assert (scanner.una.decimal_notification == '.')
        assert (scanner.una.release_character == '?')
        assert (scanner.una.segment_terminator == '\'')
        assert (!scanner.atEnd)
    }

    test ("ParseSegmentwithUNA")
    {
        val scanner = SegmentScanner ("UNA:+.? 'UNB'")
        val unb = scanner.first
        assertResult ("UNB", "segment incorrect")(unb)
    }

    test ("ParseSegmentwithoutSegmentTerminator")
    {
        val scanner = SegmentScanner ("UNB")
        val unb = scanner.first
        assertResult ("UNB", "segment incorrect")(unb)
    }

    test ("ParseSegmentWithEscapedDefaultReleaseCharacter")
    {
        val scanner = SegmentScanner ("XY?'Z'A")
        assert (scanner.una.component_data_element_separator == ':')
        assert (scanner.una.data_element_separator == '+')
        assert (scanner.una.decimal_notification == '.')
        assert (scanner.una.release_character == '?')
        assert (scanner.una.segment_terminator == '\'')
        val seg = scanner.first
        assertResult ("XY'Z", "segment incorrect")(seg)
        assert (!scanner.atEnd)
    }

    test ("ParseSegmentWithNondefaultReleaseCharacter")
    {
        val scanner = SegmentScanner ("UNA:+.\\ 'XY\\'Z'A")
        assert (scanner.una.component_data_element_separator == ':')
        assert (scanner.una.data_element_separator == '+')
        assert (scanner.una.decimal_notification == '.')
        assert (scanner.una.release_character == '\\')
        assert (scanner.una.segment_terminator == '\'')
        val seg = scanner.first
        assertResult ("XY'Z", "segment incorrect")(seg)
        assert (!scanner.atEnd)    }

    test ("ParseMultipleSegments")
    {
        val scanner = SegmentScanner ("UNA:+.? 'FOO'BAR'")
        val message = SegmentParser (scanner.una)
        val segments = message.segment.*
        segments.apply (scanner) match
        {
            case message.Success (result: List[Segment], rest) =>
                assert (result.length == 2)
                assert (result.head.name == "FOO")
                assert (result.tail.head.name == "BAR")
                assert (rest.atEnd)
            case message.Failure (msg, _) =>
                fail (s"parse failure: $msg")
            case message.Error (msg, _) =>
                fail (s"parse error: $msg")
        }
    }

    test ("ParseMultipleSegmentsTruncated")
    {
        val scanner = SegmentScanner ("UNA:+.? 'FOO'BA")
        val message = SegmentParser (scanner.una)
        val segments = message.segment.*
        val r = segments.apply (scanner)
        r match
        {
            case message.Success (result: List[Segment], _) =>
                result.foreach ((x: Segment) => println (x.name))
                fail (s"parse should fail")
            case message.Failure (msg, _) =>
                fail (s"parse error: $msg")
            case message.Error (msg, _) =>
                assert (msg.startsWith ("segment name not found"), "error message is wrong")
        }
    }

    test ("ParseSample")
    {
        val before = System.nanoTime

        val path = FileSystems.getDefault.getPath ("data/MSCONS_LG_12X-0000000858-F_12X-0000000858-F_20140314_1407300597853.txt")
        val file = FileChannel.open (path, StandardOpenOption.READ)
        val size = file.size ()
        val buffer = file.map (FileChannel.MapMode.READ_ONLY, 0L, size)
        file.close ()

        val scanner = SegmentScanner (buffer)
        val message = SegmentParser (scanner.una)
        val segments = message.segment.*
        segments.apply (scanner) match
        {
            case message.Success (result: List[Segment], _) =>
                assertResult ("UNB", "name incorrect") (result.head.name)
            case message.Failure (msg, _) =>
                fail (msg)
            case message.Error (msg, _) =>
                fail (msg)
        }

        val after = System.nanoTime
        info ("reading %d bytes took %g seconds".format (size, (after - before) / 1e9))
    }

    //    test ("data with release character")
    //    {
    //        val buffer = ByteBuffer.wrap ("UNA:+.? 'DTM+163:200901010000?+01:303'".getBytes)
    //        val mscons = new MSCONS (buffer)
    //        mscons.parseUNA ()
    //        val segs = mscons.parseAll (mscons.buffer, mscons.segment_terminator)
    //        assert (segs.length == 1)
    //        assert (mscons.buffer.remaining == 0)
    //        val data = mscons.parseData (segs.head)
    //        assert (data.length == 2)
    //        assert (segToString (data.head) == "DTM")
    //        assert (segToString (data.tail.head) == "163:200901010000+01:303")
    //    }
}

