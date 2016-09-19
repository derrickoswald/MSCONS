package ch.ninecode.mscons

import java.io.FileInputStream
import java.io.BufferedInputStream
import java.nio.channels.FileChannel
import java.nio.file.Path
import java.nio.file.StandardOpenOption
import java.nio.file.FileSystems
import java.nio.ByteBuffer

// was: BufferedInputStream
class MSCONS (val buffer: ByteBuffer) extends Serializable
{
    // See http://www.unece.org/trade/untdid/d99a/trmd/mscons_c.htm
    // and also see http://www.unece.org/cefact/edifact/welcome.html
    // and http://www.edi-energy.de/files2/MSCONS_MIG_2_2e_Lesefassung_2015_09_15_2015_09_11.pdf from Bundesverbandes der Energie- und Wasserwirtschaft e.V. ("BDEW" https://bdew.de)

    var component_data_element_separator = ":".codePointAt (0)
    var data_element_separator = "+".codePointAt (0)
    var decimal_notification = ".".codePointAt (0)
    var release_character = "?".codePointAt (0)
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

    /**
     * Extract one segment
     */
    def parseSegment (terminator: Int): ByteBuffer =
    {
        var length = buffer.limit
        var skip = false
        var stop = false
        var start = buffer.position
        var size = 0
        var c = 0
        var intervals = List[Tuple2[Int,Int]] () // start and size of each piece of the segment
        var ret: ByteBuffer = null

        do
        {
            c = buffer.get.asInstanceOf[Int]
            size += 1
            stop = false
            if (skip)
            {
                if (c == terminator)
                {
                    intervals = intervals :+ (start, size - 2)
                    start = start + size - 1
                    size = 1
                }
                skip = false
            }
            else
                if (c == terminator)
                    stop = true
                else
                    if (c == release_character)
                        skip = true
        }
        while ((size <= length) && !stop)
        intervals = intervals :+ (start, if (stop) size - 1 else size)

        if (1 == intervals.length)
        {
            // just slice out the one piece to avoid reallocation
            buffer.position (intervals.head._1)
            ret = buffer.slice ()
            ret.limit (intervals.head._2)
            buffer.position (intervals.head._1 + intervals.head._2 + (if (stop) 1 else 0))
        }
        else
        {
            // copy the data piece by piece to a new array
            val bytes = new Array[Byte] (intervals.map (_._2).sum)
            var offset = 0
            intervals.map (
                (item) =>
                {
                    buffer.position (item._1)
                    buffer.get (bytes, offset, item._2)
                    offset += item._2
                }
            )
            if (stop)
                buffer.get // discard terminator
            ret = ByteBuffer.wrap (bytes)
        }

        return (ret)
    }

    // Check for summary section
    // optional CNT Control totals (99)
    // mandatory single UNT Message trailer

    val TrailerTrigger1 = "UNT".getBytes ("UTF-8")
    val TrailerTrigger2 = "UNZ".getBytes ("UTF-8")
    val TrailerTrigger3 = "CNT".getBytes ("UTF-8")

    def isTrailer (seg: ByteBuffer): Boolean =
    {
        seg.mark
        val b0 = seg.get ()
        val b1 = seg.get ()
        val b2 = seg.get ()
        seg.reset

        var ret = false

        if (b0 == TrailerTrigger1(0))
        {
            if (b1 == TrailerTrigger1(1))
                if (b2 == TrailerTrigger1(2))
                    ret = true
        }
        else if (b0 == TrailerTrigger2(0))
        {
            if (b1 == TrailerTrigger2(1))
                if (b2 == TrailerTrigger2(2))
                    ret = true
        }
        else if (b0 == TrailerTrigger3(0))
        {
            if (b1 == TrailerTrigger3(1))
                if (b2 == TrailerTrigger3(2))
                    ret = true
        }

        return (ret)
    }

    def parseReadings ()
    {
        var done = false // goes true when we're done with the file
        var started = false // goes true when message start (UNS) has been seen
        var repeat = false // when true, repeat switch statement with same segment
        var name = ""
        var state = 0
        var record = null
        var seg: ByteBuffer = null
        do
        {
            if (repeat)
                repeat = false
            else
                // isolate the next message segment
                seg = parseSegment (segment_terminator)


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
////                if ((null != record) && (typeof (callback) == "function"))
////                    callback (record)
//            }
//            else
//            {
//                if (started)
//                    switch (state)
//                    {
//                        case 0:
//                            throw "illegal state exception"
//                            break
//
//                        case 170: // NAD
//                            name = ParseNameAndAddress (seg)
//                            state = 190
//                            break
//
//                        case 190: // mandatory LOC
//                            record = {}
//                            record["name"] = name
//                            record["location"] = ParseLocation (seg)
//                            record["quantities"] = []
//                            state = 200
//                            break
//
//                        case 200: // optional DTM (up to 9)
//                            if (isDateTimeOrPeriod (seg))
//                            {
//                                var val = ParseDateTimeOrPeriod (seg)
//                                if (typeof (val) == "object")
//                                    record.date = val
//                                else
//                                    record.interval = val
//                                // state = 200 // again
//                                break
//                            }
//                            else
//                            {
//                                // optional or no more DTM found
//                                state = 250
//                                repeat = true
//                            }
//                            break
//
//                            // TODO: should really handle optional group 7 (RFF)
//
//                        case 250: // optional group 8 with mandatory CCI
//                            if (isCCIItem (seg))
//                            {
//                                record.characteristic = ParseCharacteristic (seg)
//                                state = 260
//                            }
//                            else
//                            {
//                                state = 280
//                                repeat = true
//                            }
//                            break
//
//                        case 260: // optional DTM (up to 99)
//                            if (isDateTimeOrPeriod (seg))
//                            {
//                                record["characteristic_date"] = record["characteristic_date"] || []
//                                record["characteristic_date"].push (ParseDateTimeOrPeriod (seg))
//                                break
//                            }
//                            else
//                            {
//                                // optional or no more DTM found
//                                state = 280
//                                repeat = true
//                            }
//                            break
//
//                        case 280: // optional group 9 with mandatory LIN
//                            if (isLineItem (seg))
//                            {
//                                record.item = ParseLineItem (seg)
//                                state = 290
//                            }
//                            else
//                            {
//                                // optional group 9 (LIN) not found
//                                state = 350
//                                repeat = true
//                            }
//                            break
//
//                        case 290: // optional PIA
//                            if (isProductId (seg))
//                                record.product = ParseProduct (seg)
//                            else
//                                // optional PIA not found
//                                repeat = true
//                            state = 350
//                            break
//
//                        case 350: // mandatory quantity
//                            if (isQuantity (seg))
//                                record.quantities.push (ParseQuantity (seg))
//                            else
//                            {
//                                // TODO: handle DTM
//
//                                // process the complete record
//                                if (typeof (callback) == "function")
//                                    callback (record)
//
//                                record = null
//
//                                state = 190
//                                repeat = true
//                            }
//                            break
//                    }
//                else
//                    if (IsMessageStart (seg))
//                    {
//                        started = true
//                        state = 170
//                    }
            }
        }
        while (!done)
    }
}

object MSCONS
{
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
            mscons.parseReadings ()

            val after = System.nanoTime
            println ("reading %g seconds".format ((after - before) / 1e9))
        }
        else
            println ("MSCONS input file not specified")
    }
}
