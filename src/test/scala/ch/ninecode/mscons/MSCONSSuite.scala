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
        assert (mscons.component_data_element_separator == ':')
        assert (mscons.data_element_separator == '+')
        assert (mscons.decimal_notification == '.')
        assert (mscons.release_character == '?')
        assert (mscons.segment_terminator == '\'')
    }

    test ("UNA 2 - read default")
    {
        val buffer = ByteBuffer.wrap ("UNA:+.? 'X".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        assert (mscons.component_data_element_separator == ':')
        assert (mscons.data_element_separator == '+')
        assert (mscons.decimal_notification == '.')
        assert (mscons.release_character == '?')
        assert (mscons.segment_terminator == '\'')
        assert (mscons.buffer.remaining == 1)
    }

    test ("UNA 3 - read non-default")
    {
        val buffer = ByteBuffer.wrap ("UNA:+,? 'X".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        assert (mscons.component_data_element_separator == ':')
        assert (mscons.data_element_separator == '+')
        assert (mscons.decimal_notification == ',')
        assert (mscons.release_character == '?')
        assert (mscons.segment_terminator == '\'')
        assert (mscons.buffer.remaining == 1)
    }

    test ("UNA 4 - terminator")
    {
        val buffer = ByteBuffer.wrap ("UNA:+.? ;XYZ;0".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        assert (mscons.component_data_element_separator == ':')
        assert (mscons.data_element_separator == '+')
        assert (mscons.decimal_notification == '.')
        assert (mscons.release_character == '?')
        assert (mscons.segment_terminator == ';')
        val seg = mscons.parse (mscons.buffer, mscons.segment_terminator)
        assert (segToString (seg) == "XYZ")
        assert (mscons.buffer.remaining == 1)
    }

    test ("UNA 5 - none")
    {
        val buffer = ByteBuffer.wrap ("XYZ'".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        assert (mscons.component_data_element_separator == ':')
        assert (mscons.data_element_separator == '+')
        assert (mscons.decimal_notification == '.')
        assert (mscons.release_character == '?')
        assert (mscons.segment_terminator == '\'')
        assert (mscons.buffer.remaining == 4)
    }

    test ("UNA 6 - none no terminator")
    {
        val buffer = ByteBuffer.wrap ("XYZ;".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        assert (mscons.component_data_element_separator == ':')
        assert (mscons.data_element_separator == '+')
        assert (mscons.decimal_notification == '.')
        assert (mscons.release_character == '?')
        assert (mscons.segment_terminator == '\'')
        assert (mscons.buffer.remaining == 4)
    }

    test ("segment with release character default")
    {
        val buffer = ByteBuffer.wrap ("XY?'Z'A".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        assert (mscons.component_data_element_separator == ':')
        assert (mscons.data_element_separator == '+')
        assert (mscons.decimal_notification == '.')
        assert (mscons.release_character == '?')
        assert (mscons.segment_terminator == '\'')
        val seg = mscons.parse (mscons.buffer, mscons.segment_terminator)
        assert (segToString (seg) == "XY'Z")
        assert (mscons.buffer.remaining == 1)
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
        assert (segToString (seg) == "XY'Z")
        assert (mscons.buffer.remaining == 1)
    }

    test ("multi-segment")
    {
        val buffer = ByteBuffer.wrap ("UNA:+.? 'FOO'BAR'".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        val segs = mscons.parseAll (mscons.buffer, mscons.segment_terminator)
        assert (segs.length == 2)
        assert (mscons.buffer.remaining == 0)
        assert (segToString (segs.head) == "FOO")
        assert (segToString (segs.tail.head) == "BAR")
    }

    test ("multi-segment truncated")
    {
        val buffer = ByteBuffer.wrap ("UNA:+.? 'FOO'BA".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        val segs = mscons.parseAll (mscons.buffer, mscons.segment_terminator)
        assert (segs.length == 2)
        assert (mscons.buffer.remaining == 0)
        assert (segToString (segs.head) == "FOO")
        assert (segToString (segs.tail.head) == "BA")
    }

    test ("data with release character")
    {
        val buffer = ByteBuffer.wrap ("UNA:+.? 'DTM+163:200901010000?+01:303'".getBytes)
        val mscons = new MSCONS (buffer)
        mscons.parseUNA ()
        val segs = mscons.parseAll (mscons.buffer, mscons.segment_terminator)
        assert (segs.length == 1)
        assert (mscons.buffer.remaining == 0)
        val data = mscons.parseData (segs.head)
        assert (data.length == 2)
        assert (segToString (data.head) == "DTM")
        assert (segToString (data.tail.head) == "163:200901010000+01:303")
    }

}
