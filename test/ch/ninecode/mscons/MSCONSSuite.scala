package ch.ninecode.mscons

import java.nio.ByteBuffer

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

class MSCONSSuite extends FunSuite
{
    test ("UNA 1 - default")
    {
        val buffer = ByteBuffer.wrap ("".getBytes)
        val mscons = new MSCONS (buffer)
        assert (':' == mscons.component_data_element_separator)
        assert ('+' == mscons.data_element_separator)
        assert ('.' == mscons.decimal_notification)
        assert ('?' == mscons.release_character)
        assert ('\'' == mscons.segment_terminator)
    }

    test ("UNA 2 - read default")
    {
        val buffer = ByteBuffer.wrap ("UNA:+.? 'X".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        assert (':' == mscons.component_data_element_separator)
        assert ('+' == mscons.data_element_separator)
        assert ('.' == mscons.decimal_notification)
        assert ('?' == mscons.release_character)
        assert ('\'' == mscons.segment_terminator)
        assert (1 == mscons.buffer.remaining)
    }

    test ("UNA 3 - read non-default")
    {
        val buffer = ByteBuffer.wrap ("UNA:+,? 'X".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        assert (':' == mscons.component_data_element_separator)
        assert ('+' == mscons.data_element_separator)
        assert (',' == mscons.decimal_notification)
        assert ('?' == mscons.release_character)
        assert ('\'' == mscons.segment_terminator)
        assert (1 == mscons.buffer.remaining)
    }

    test ("UNA 4 - terminator")
    {
        val buffer = ByteBuffer.wrap ("UNA:+.? ;XYZ;0".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        assert (':' == mscons.component_data_element_separator)
        assert ('+' == mscons.data_element_separator)
        assert ('.' == mscons.decimal_notification)
        assert ('?' == mscons.release_character)
        assert (';' == mscons.segment_terminator)
        val seg = mscons.parseSegment (mscons.segment_terminator)
        assert ("XYZ" == new String (seg, "UTF-8"))
        assert (1 == mscons.buffer.remaining)
    }

    test ("UNA 5 - none")
    {
        val buffer = ByteBuffer.wrap ("XYZ'".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        assert (':' == mscons.component_data_element_separator)
        assert ('+' == mscons.data_element_separator)
        assert ('.' == mscons.decimal_notification)
        assert ('?' == mscons.release_character)
        assert ('\'' == mscons.segment_terminator)
        assert (4 == mscons.buffer.remaining)
    }

    test ("UNA 6 - none no terminator")
    {
        val buffer = ByteBuffer.wrap ("XYZ;".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        assert (':' == mscons.component_data_element_separator)
        assert ('+' == mscons.data_element_separator)
        assert ('.' == mscons.decimal_notification)
        assert ('?' == mscons.release_character)
        assert ('\'' == mscons.segment_terminator)
        assert (4 == mscons.buffer.remaining)
    }

    test ("Segment with release character default")
    {
        val buffer = ByteBuffer.wrap ("XY?'Z'A".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        assert (':' == mscons.component_data_element_separator)
        assert ('+' == mscons.data_element_separator)
        assert ('.' == mscons.decimal_notification)
        assert ('?' == mscons.release_character)
        assert ('\'' == mscons.segment_terminator)
        val seg = mscons.parseSegment (mscons.segment_terminator)
        assert ("XY'Z" == new String (seg, "UTF-8"))
        assert (1 == mscons.buffer.remaining)
    }

    test ("Segment with release character special")
    {
        val buffer = ByteBuffer.wrap ("UNA:+.\\ 'XY\\'Z'A".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        assert (':' == mscons.component_data_element_separator)
        assert ('+' == mscons.data_element_separator)
        assert ('.' == mscons.decimal_notification)
        assert ('\\' == mscons.release_character)
        assert ('\'' == mscons.segment_terminator)
        val seg = mscons.parseSegment (mscons.segment_terminator)
        assert ("XY'Z" == new String (seg, "UTF-8"))
        assert (1 == mscons.buffer.remaining)
    }

}
