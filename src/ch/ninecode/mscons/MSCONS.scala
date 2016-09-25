package ch.ninecode.mscons

import java.io.FileInputStream
import java.io.BufferedInputStream
import java.nio.channels.FileChannel
import java.nio.file.Path
import java.nio.file.StandardOpenOption
import java.nio.file.FileSystems
import java.nio.ByteBuffer
import java.util.regex.Pattern

class MSCONS (val buffer: ByteBuffer) extends Serializable
{
    // stupid funky scala syntax to "import" the companion object constant declarations
    import MSCONS._

    // For a description of EDIFACT message application level syntax rules, see http://www.unece.org/trade/untdid/texts/d422_d.htm
    // For a descripion of MSCONS messages, see http://www.unece.org/trade/untdid/d09b/trmd/mscons_c.htm
    //   ... or a older one used to write the original Javascript code http://www.unece.org/trade/untdid/d99a/trmd/mscons_c.htm
    // The United Nations group responsible for EDIFACT see http://www.unece.org/cefact/edifact/welcome.html
    // For a description (auf Deutsch) of MSCONS messages used/implemented by Bundesverbandes der Energie- und Wasserwirtschaft e.V. ("BDEW" https://bdew.de) see http://www.edi-energy.de/files2/MSCONS_MIG_2_2e_Lesefassung_2015_09_15_2015_09_11.pdf

    var component_data_element_separator = ":".codePointAt (0)
    var data_element_separator = "+".codePointAt (0)
    var decimal_notification = ".".codePointAt (0)
    var release_character = "?".codePointAt (0) // ToDo: a space character means the release character is not used
    var segment_terminator = "'".codePointAt (0)

    /**
     * Convert a ByteBuffer into a string using UTF-8 encoding.
     * A convenience function to drain the buffer as if it was a
     * UTF-8 encoded string.
     * @param buffer - the buffer to convert, which will be exhausted
     * @return the string value of the buffer as UTF-8 characters
     */
    def segToString (buffer: ByteBuffer): String =
    {
        val ret = if (buffer.hasArray && !buffer.isReadOnly)
            new String (buffer.array, buffer.arrayOffset, buffer.limit, "UTF-8")
        else
        {
            val bytes = new Array[Byte] (buffer.remaining)
            buffer.get (bytes)
            new String (bytes, "UTF-8")
        }
        return (ret)
    }

    /**
     * Convert a ByteBuffer into an integer.
     * A convenience function to drain the buffer as if it was a
     * UTF-8 encoded string representation of a decimal integer.
     * @param buffer - the buffer to convert, which will be exhausted
     * @return the integer value of the buffer
     */
    def segToInteger (buffer: ByteBuffer): Int =
    {
        var ret = 0
        while (0 < buffer.remaining)
            ret = ret * 10 + (buffer.get () - '0')
        return (ret)
    }

    /**
     * Convert a ByteBuffer into a double.
     * A convenience function to drain the buffer as if it was a
     * UTF-8 encoded string representation of a floating point decimal number.
     * @param buffer - the buffer to convert, which will be exhausted
     * @return the floating point value of the buffer
     */
    def segToDouble (buffer: ByteBuffer): Double =
    {
        var ret = segToString (buffer).replace (decimal_notification.toChar, '.').toDouble

        return (ret)
    }

    def debugPrint (buffer: ByteBuffer): Unit =
    {
        buffer.mark
        println (segToString (buffer))
        buffer.reset
    }

    // Parse the optional UNA segment if present.
    // See http://en.wikipedia.org/wiki/EDIFACT
    // There are six characters following UNA in this order (default in brackets):
    //     component data element separator (:)
    //     data element separator (+)
    //     decimal notification (.)
    //     release character (?)
    //     reserved, must be a space
    //     segment terminator (')
    def parseUNA (): Unit =
    {
        if (buffer.limit >= 9)
        {
            buffer.mark
            val buna = new Array[Char] (9)
            for (i <- 0 until 9)
                buna(i) = buffer.get.asInstanceOf[Char]
            var una = new String (buna)
            if (("UNA" == una.substring (0, 3)) && (" " == una.substring (7, 8)))
            {
                component_data_element_separator = una.substring (3, 4).codePointAt (0)
                data_element_separator = una.substring (4, 5).codePointAt (0)
                decimal_notification = una.substring (5, 6).codePointAt (0)
                release_character = una.substring (6, 7).codePointAt (0)
                segment_terminator = una.substring (8).codePointAt (0)
            }
            else
                buffer.reset ()
        }
    }

    def parseUNB (): Unit =
    {
        val seg = parse (buffer, segment_terminator)
        // UNB+UNOC:3+12X-0000000858-F:500+12XBKW-HANDEL--X:500+140304:1854+1406300423489++LG
        val elements = parseData (seg)
        if (predicate (elements.head, UNB))
        {
            val syntax = parseComponents (elements.tail.head)
            if (("UNOC".equals (segToString (syntax.head))) && (3 == segToInteger (syntax.tail.head)))
            {
                // 12X-0000000858-F:500
                val sender = parseComponents (elements.tail.tail.head)
                // 12XBKW-HANDEL--X:500
                val recipient = parseComponents (elements.tail.tail.tail.head)
                // 140304:1854
                val datetime = parseComponents (elements.tail.tail.tail.tail.head)
                // 1406300423489
                val reference = elements.tail.tail.tail.tail.tail.head
                println ("from: " + segToString (sender.head))
                println ("to: " + segToString (recipient.head))
                println ("date: " + segToString (datetime.head))
                println ("time: " + segToString (datetime.tail.head))
                println ("reference: " + segToString (reference))
                // 14  EAN International
                // 500 DE, BDEW (Bundesverband der Energie- und Wasserwirtschaft e.V.)
                // 501 EASEE gas (European Association for the Streamlining of Energy Exchange)
                // 502 DE, DVGW (Deutsche Vereinigung des Gas-und Wasserfaches e.V.)
                // ZZZ ETSO
            }
            else
                throw new Exception ("message does not conform to UN/ECE level C version 3")
        }
        else
            throw new Exception ("expected UNB segment not found")
    }

    /**
     * Extract one segment
     */
    def parse (buf: ByteBuffer, term: Int): ByteBuffer =
    {
        var skip = false
        var stop = false
        var start = buf.position
        var size = 0
        var c = 0
        var intervals = List[Tuple2[Int,Int]] () // start and size of each piece of the segment
        var ret: ByteBuffer = null

        while ((0 < buf.remaining) && !stop)
        {
            c = buf.get.asInstanceOf[Int]
            size += 1
            stop = false
            if (skip)
            {
                if (c == term)
                {
                    intervals = intervals :+ (start, size - 2)
                    start = start + size - 1
                    size = 1
                }
                skip = false
            }
            else
                if (c == term)
                    stop = true
                else
                    if (c == release_character)
                        skip = true
        }

        intervals = intervals :+ (start, if (stop) size - 1 else size)

        if (1 == intervals.length)
        {
            // just slice out the one piece to avoid reallocation
            buf.position (intervals.head._1)
            ret = buf.slice ()
            ret.limit (intervals.head._2)
            buf.position (intervals.head._1 + intervals.head._2 + (if (stop) 1 else 0))
        }
        else
        {
            // copy the data piece by piece to a new array
            val bytes = new Array[Byte] (intervals.map (_._2).sum)
            var offset = 0
            intervals.map (
                (item) =>
                {
                    buf.position (item._1)
                    buf.get (bytes, offset, item._2)
                    offset += item._2
                }
            )
            if (stop)
                buf.get // discard terminator
            ret = ByteBuffer.wrap (bytes)
        }

        return (ret)
    }

    // Check for summary section
    // optional CNT Control totals (99)
    // mandatory single UNT Message trailer

    def isTrailer (seg: ByteBuffer): Boolean =
    {
        seg.mark
        val b0 = seg.get ()
        val b1 = seg.get ()
        val b2 = seg.get ()
        seg.reset

        var ret = false

        if (b0 == UNT(0))
        {
            if (b1 == UNT(1))
                if (b2 == UNT(2))
                    ret = true
        }
        else if (b0 == UNZ(0))
        {
            if (b1 == UNZ(1))
                if (b2 == UNZ(2))
                    ret = true
        }
        else if (b0 == CNT(0))
        {
            if (b1 == CNT(1))
                if (b2 == CNT(2))
                    ret = true
        }

        return (ret)
    }

    def predicate (seg: ByteBuffer, pattern: Array[Byte]): Boolean =
    {
        var ret = false

        seg.mark
        val b0 = seg.get ()
        val b1 = seg.get ()
        val b2 = seg.get ()
        seg.reset

        if (b0 == pattern(0))
            if (b1 == pattern(1))
                if (b2 == pattern(2))
                    ret = true

        return (ret);
    }

    def isMessageStart (seg: ByteBuffer): Boolean = { predicate (seg, UNS) }
    def isDateTimeOrPeriod (seg: ByteBuffer): Boolean = { predicate (seg, DTM) }
    def isCCIItem (seg: ByteBuffer): Boolean = { predicate (seg, CCI) }
    def isLineItem (seg: ByteBuffer): Boolean = { predicate (seg, LIN) }
    def isProductId (seg: ByteBuffer): Boolean = { predicate (seg, PIA) }
    def isQuantity (seg: ByteBuffer): Boolean = { predicate (seg, QTY) }

    def parseAll (seg: ByteBuffer, terminator: Int): List[ByteBuffer] =
    {
        var ret = List[ByteBuffer] ()

        while (0 < seg.remaining)
            ret = ret :+ parse (seg, terminator)

        return (ret)
    }

    def parseData (seg: ByteBuffer): List[ByteBuffer] = parseAll (seg, data_element_separator)

    def parseComponents (seg: ByteBuffer): List[ByteBuffer] = parseAll (seg, component_data_element_separator)

    def ParseNameAndAddress (seg: ByteBuffer): String =
    {
        // http://www.unece.org/trade/untdid/d99a/trsd/trsdnad.htm
        // NAD+HN+12X-0000000858-F::293
        // HN = Service performer (the party who is performing a service).
        // 12X-0000000858-F = Code identifying a party involved in a transaction.
        // 293 = Code identifying the agency responsible for a code list. ZZZ = Mutually defined
        val elements = parseData (seg)
        val parts = parseComponents (elements.tail.head)

        return (segToString (parts.head))
    }

    def ParseLocation (seg: ByteBuffer): String =
    {
        // LOC+172+::87:CH1022201234500000000000000022079
        val elements = parseData (seg)
        val parts = parseComponents (elements.tail.tail.head)

        return (segToString (parts.tail.tail.tail.head))
    }

    def ParseDateTimeOrPeriod (seg: ByteBuffer): String =
    {
        var ret = ""

        // DTM+163:201303240000?+01:303
        // DTM+672:15:806
        val elements = parseData (seg)
        val parts = parseComponents (elements.tail.head)

        // TODO: handle more qualifiers? http://www.unece.org/trade/untdid/d99a/uncl/uncl2005.htm
        val qualifier = segToInteger (parts.head)
        val value = segToString (parts.tail.head)
        val fmt = segToInteger (parts.tail.tail.head)
        qualifier match
        {
            case 163 =>
                if (303 != fmt)
                    throw new Exception ("unrecognized date format")
                val matcher = date303.matcher (value)
                if (matcher.find)
                {

                    val s = MonthNames (matcher.group(2).toInt) + " " + matcher.group(3) + ", " + matcher.group(1) + " " + matcher.group(4) + ":" + matcher.group(5) + ":00 GMT" + matcher.group(6) + ":00"
                    ret = s // new Date (s);
                }

            case 672 =>
                if (806 != fmt)
                    throw new Exception ("unrecognized interval format")
                ret = value

            case _ =>
                ret = "0"
        }

        return (ret)
    }

    def ParseCharacteristic (seg: ByteBuffer): String =
    {
        // CCI+10++WS::293
        val elements = parseData (seg)
        val cls = segToInteger (elements.tail.head)
        val details = segToString (elements.tail.tail.head)
        val characteristic = if (0 == elements.tail.tail.tail.head.remaining)
            ""
        else
        {
            val parts = parseComponents (elements.tail.tail.tail.head);
            val identification = segToString (parts.head)
            val agency = segToString (parts.tail.tail.head)
            "{identification: " + identification + ", agency: " + agency + "}"
        }
        val relevance = segToString (elements.tail.tail.tail.head)
        val ret = "{property_class: " + cls + ", details: " + details + ", characteristic: " + characteristic + ", relevance: " + relevance + "}"

        return (ret);
    }

    def ParseLineItem (seg: ByteBuffer): Int =
    {
        // LIN+1
        val elements = parseData (seg);

        return (segToInteger (elements.tail.head))
    }

    /**
     * Parse an OBIS code in the additional product id (PIA) segment.
     *
     * For PIA product code:
     * @see http://www.unece.org/trade/untdid/d99a/uncl/uncl4347.htm
     *
     * The OBIS code consists of (up to) 6 group sub-identifiers marked
     * by letters A to F. All these may or may not be present in the
     * identifier (e.g. groups A and B are often omitted).
     * In order to decide to which group the sub-identifier belongs,
     * the groups are separated by unique separators:
     *
     *   A-B:C.D.E*F
     *
     *   - The A group defines the medium
     *     (0=abstract objects, 1=electricity, 6=heat, 7=gas, 8=water, ...)
     *   - The B group defines the channel.
     *     Each device with multiple channels generating measurement results,
     *     can separate the results into the channels.
     *   - The C group defines the physical value
     *     (current, voltage, energy, level, temperature, ...)
     *   - The D group defines the quantity computation output of specific algorithm
     *   - The E group specifies the measurement type defined by
     *     groups A to D into individual measurements (e.g. switching ranges)
     *   - The F group separates the results partly defined by groups A to E.
     *     The typical usage is the specification of individual time ranges.
     *
     * Reduced ID codes (e.g. for IEC 62056-21 )
     * To comply with the syntax defined for protocol modes A to D of IEC 62056-21, the range of ID
     * codes is reduced to fulfil the limitations which are usually applied to the number of digits and
     * the ASCII representation of them. All value groups are limited to a range of 0 .. 99 and within
     * that range, to the limits given in the relevant chapters.
     * Some value groups may be suppressed, if they are not relevant to an application:
     *     Optional value groups: A, B, E, F
     *     Mandatory value groups: C, D
     * To allow the interpretation of shortened codes delimiters are
     * inserted between all value groups:
     *
     *   A-B:C.D.E*F
     *
     * The delimiter between value groups E and F can be modified to carry some information about
     * the source of a reset (& instead of * if the reset was performed manually).
     * For compatibility with existing implementations, in value group A an identifier for an energy
     * type may be used even for abstract objects.
     *
     * For CIM mapping could use:
     *   Electricity metering data exchange – The DLMS/COSEM suite –
     *   Part 6-9: Mapping between the Common Information Model message profiles
     *   (IEC 61968-9) and DLMS/COSEM (IEC 62056) data models and protocols
     */
    def ParseProduct (seg: ByteBuffer): String =
    {
        // PIA+5+1-1?:1.29.0*255:SWR
        val elements = parseData (seg)
        val product = segToInteger (elements.tail.head) // 5 = Product identification
        val parts = parseComponents (elements.tail.tail.head)
        // Medium - Kanal : Messgrösse . Messart . Tarif * Vorwert
        val s = segToString (parts.head)
        val pieces = s.split ("""[\-\:\.\*]""")
        val ret =
        "{" +
            "medium: " + pieces(0).toInt + ", " +
            "kanal: " + pieces(1).toInt + ", " +
            "messgrösse: " + pieces(2).toInt + ", " +
            "messart: " + pieces(3).toInt + ", " +
            "tarif: " + pieces(4).toInt + ", " +
            "vorwert: " + pieces(5).toInt +
        "}"

        return (ret);
    }

    def ParseQuantity (seg: ByteBuffer): Double =
    {
        // QTY+46:20.800:KWH
        val elements = parseData (seg)
        val parts = parseComponents (elements.tail.head)

        return (segToDouble (parts.tail.head))
    }

    case class Record (
        name: String,
        location: String,
        var quantities: List[Double],
        var date: String = "",
        var interval: String = "",
        var characteristic: String = "",
        var characteristic_date: Array[String] = null,
        var item: Int = 0,
        var product: String = ""
        )

    def parseReadings ()
    {
        var done = false // goes true when we're done with the file
        var started = false // goes true when message start (UNS) has been seen
        var repeat = false // when true, repeat switch statement with same segment
        var name = ""
        var state = 0
        var record: Record = null
        var seg: ByteBuffer = null
        do
        {
            if (repeat)
                repeat = false
            else
                // isolate the next message segment
                seg = parse (buffer, segment_terminator)


            // TODO: make into a state machine as an array of objects with two methods operate(segment) and step(n)

            /*
            DETAIL SECTION

    0150   UNS Section control                           M   1

    0160             Segment group 5                     M   99999
    0170   NAD Name and address                          M   1

    0180             Segment group 6                     M   99999
    0190   LOC Place/location identification             M   1
    0200   DTM Date/time/period                          C   9

    0210             Segment group 7                     C   99
    0220   RFF Reference                                 M   1
    0230   DTM Date/time/period                          C   9

    0240             Segment group 8                     C   99
    0250   CCI Characteristic/class id                   M   1
    0260   DTM Date/time/period                          C   99

    0270             Segment group 9                     C   99999
    0280   LIN Line item                                 M   1
    0290   PIA Additional product id                     C   9
    0300   IMD Item description                          C   9
    0310   PRI Price details                             C   9
    0320   NAD Name and address                          C   9
    0330   MOA Monetary amount                           C   9

    0340             Segment group 10                    M   9999
    0350   QTY Quantity                                  M   1
    0360   DTM Date/time/period                          C   9

    0370             Segment group 11                    C   99
    0380   CCI Characteristic/class id                   M   1
    0390   MEA Measurements                              C   99
    0400   DTM Date/time/period                          C   9

             */

            if (isTrailer (seg))
            {
                done = true
                // process any partially complete record
//                if ((null != record) && (typeof (callback) == "function"))
//                    callback (record)
            }
            else
            {
                if (started)
                    state match
                    {
                        case 0 =>
                            throw new Exception ("illegal state exception")

                        case 170 =>// NAD
                            name = ParseNameAndAddress (seg)
                            state = 190

                        case 190 => // mandatory LOC
                            record = Record (name, ParseLocation (seg), List[Double] ())
                            state = 200

                        case 200 => // optional DTM (up to 9)
                            if (isDateTimeOrPeriod (seg))
                            {
                                var v = ParseDateTimeOrPeriod (seg)
//                                if (typeof (v) == "object")
                                    record.date = v
//                                else
//                                    record.interval = v
                                // state = 200 // again
                            }
                            else
                            {
                                // optional or no more DTM found
                                state = 250
                                repeat = true
                            }

                            // TODO: should really handle optional group 7 (RFF)

                        case 250 => // optional group 8 with mandatory CCI
                            if (isCCIItem (seg))
                            {
                                record.characteristic = ParseCharacteristic (seg)
                                state = 260
                            }
                            else
                            {
                                state = 280
                                repeat = true
                            }

                        case 260 => // optional DTM (up to 99)
                            if (isDateTimeOrPeriod (seg))
                            {
                                record.characteristic_date = record.characteristic_date :+ ParseDateTimeOrPeriod (seg)
                            }
                            else
                            {
                                // optional or no more DTM found
                                state = 280
                                repeat = true
                            }

                        case 280 => // optional group 9 with mandatory LIN
                            if (isLineItem (seg))
                            {
                                record.item = ParseLineItem (seg)
                                state = 290
                            }
                            else
                            {
                                // optional group 9 (LIN) not found
                                state = 350
                                repeat = true
                            }

                        case 290 => // optional PIA
                            if (isProductId (seg))
                                record.product = ParseProduct (seg)
                            else
                                // optional PIA not found
                                repeat = true
                            state = 350

                        case 350 => // mandatory quantity
                            if (isQuantity (seg))
                                record.quantities = record.quantities :+ ParseQuantity (seg)
                            else
                            {
                                // TODO: handle DTM

//                                // process the complete record
//                                if (typeof (callback) == "function")
//                                    callback (record)

                                record = null

                                state = 190
                                repeat = true
                            }
                    }
                else
                    if (isMessageStart (seg))
                    {
                        started = true
                        state = 170
                    }
            }
        }
        while (!done)
    }
}

object MSCONS
{
    final val UNB = "UNB".getBytes ("UTF-8")
    final val UNT = "UNT".getBytes ("UTF-8")
    final val UNZ = "UNZ".getBytes ("UTF-8")
    final val CNT = "CNT".getBytes ("UTF-8")
    final val UNS = "UNS".getBytes ("UTF-8")
    final val DTM = "DTM".getBytes ("UTF-8")
    final val CCI = "CCI".getBytes ("UTF-8")
    final val LIN = "LIN".getBytes ("UTF-8")
    final val PIA = "PIA".getBytes ("UTF-8")
    final val QTY = "QTY".getBytes ("UTF-8")
    final val date303 = Pattern.compile ("""([0-9]{1,4})([0-9]{1,2})([0-9]{1,2})([0-9]{1,2})([0-9]{1,2})([\?]*)""")
    final val MonthNames = Array[String] (
          "",
          "January",
          "February",
          "March",
          "April",
          "May",
          "June",
          "July",
          "August",
          "September",
          "October",
          "November",
          "December")


    /**
     * Main program for testing purposes.
     */
    def main (args: Array[String])
    {
        if (args.size > 0)
        {
            val before = System.nanoTime

            val path = FileSystems.getDefault ().getPath (args (0));
            val file = FileChannel.open (path, StandardOpenOption.READ)
            val buffer = file.map (FileChannel.MapMode.READ_ONLY, 0l, file.size ())
            file.close ()

            val mscons = new MSCONS (buffer)
            mscons.parseUNA ()
            mscons.parseUNB ()
            mscons.parseReadings ()

            val after = System.nanoTime
            println ("reading %g seconds".format ((after - before) / 1e9))
        }
        else
            println ("MSCONS input file not specified")
    }
}
