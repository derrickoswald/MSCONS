package ch.ninecode.mscons

import java.nio.ByteBuffer

import org.scalatest.FunSuite

class MSCONSSuite extends FunSuite
{
    /**
     * Convert a ByteBuffer into a string using UTF-8 encoding.
     * @param buffer - the buffer to convert, which will be exhausted
     * @return the string value of the buffer as UTF-8 characters
     */
    def segToString (buffer: ByteBuffer): String =
    {
        if (buffer.hasArray && !buffer.isReadOnly)
            new String (buffer.array, buffer.arrayOffset, buffer.limit, "UTF-8")
        else
        {
            val bytes = new Array[Byte] (buffer.remaining)
            buffer.get (bytes)
            new String (bytes, "UTF-8")
        }
    }

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
        val seg = mscons.parse (mscons.buffer, mscons.segment_terminator)
        assert ("XYZ" == segToString (seg))
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

    test ("segment with release character default")
    {
        val buffer = ByteBuffer.wrap ("XY?'Z'A".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        assert (':' == mscons.component_data_element_separator)
        assert ('+' == mscons.data_element_separator)
        assert ('.' == mscons.decimal_notification)
        assert ('?' == mscons.release_character)
        assert ('\'' == mscons.segment_terminator)
        val seg = mscons.parse (mscons.buffer, mscons.segment_terminator)
        assert ("XY'Z" == segToString (seg))
        assert (1 == mscons.buffer.remaining)
    }

    test ("segment with release character special")
    {
        val buffer = ByteBuffer.wrap ("UNA:+.\\ 'XY\\'Z'A".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        assert (':' == mscons.component_data_element_separator)
        assert ('+' == mscons.data_element_separator)
        assert ('.' == mscons.decimal_notification)
        assert ('\\' == mscons.release_character)
        assert ('\'' == mscons.segment_terminator)
        val seg = mscons.parse (mscons.buffer, mscons.segment_terminator)
        assert ("XY'Z" == segToString (seg))
        assert (1 == mscons.buffer.remaining)
    }

    test ("multi-segment")
    {
        val buffer = ByteBuffer.wrap ("UNA:+.? 'FOO'BAR'".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        val segs = mscons.parseAll (mscons.buffer, mscons.segment_terminator)
        assert (2 == segs.length)
        assert (0 == mscons.buffer.remaining)
        assert ("FOO" == segToString (segs.head))
        assert ("BAR" == segToString (segs.tail.head))
    }

    test ("multi-segment truncated")
    {
        val buffer = ByteBuffer.wrap ("UNA:+.? 'FOO'BA".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        val segs = mscons.parseAll (mscons.buffer, mscons.segment_terminator)
        assert (2 == segs.length)
        assert (0 == mscons.buffer.remaining)
        assert ("FOO" == segToString (segs.head))
        assert ("BA" == segToString (segs.tail.head))
    }

    test ("data with release character")
    {
        val buffer = ByteBuffer.wrap ("UNA:+.? 'DTM+163:200901010000?+01:303'".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        val segs = mscons.parseAll (mscons.buffer, mscons.segment_terminator)
        assert (1 == segs.length)
        assert (0 == mscons.buffer.remaining)
        val data = mscons.parseData (segs.head)
        assert (2 == data.length)
        assert ("DTM" == segToString (data.head))
        assert ("163:200901010000+01:303" == segToString (data.tail.head))
    }

}
