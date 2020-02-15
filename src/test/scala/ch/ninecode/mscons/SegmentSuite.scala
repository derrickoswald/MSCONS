package ch.ninecode.mscons

import java.util.Calendar

import org.scalatest.FunSuite

import ch.ninecode.edifact.Segment
import ch.ninecode.edifact.SegmentParser
import ch.ninecode.edifact.SegmentScanner
import ch.ninecode.edifact.ServiceSegmentParser

class SegmentSuite extends FunSuite
{
    val mscons = "UNB+UNOC:3+12X-SAK-N------6:500+12X-SAK-N------6:500+191215:0430+eslevu14572840++TL'UNH+slevu14572840D+MSCONS:D:04B:UN:2.2e'BGM+7+slevu14572840D+9'DTM+137:201912140000:203'"

    def parseAndCheck (text: String, check: MSCONSMessage04B => Unit): Unit =
    {
        val scanner = SegmentScanner (mscons)
        val message = SegmentParser (scanner.una)
        val segments = message.segment.*
        segments.apply (scanner) match
        {
            case message.Success (result: List[Segment], _) =>
                // result.foreach (segment => println (segment))
                val x = ServiceSegmentParser.read (result)
                x match
                {
                    case ServiceSegmentParser.Success (r, rest) =>
                        if (   (r.unh.Type == "MSCONS")
                            && (r.unh.Version == "D"))
                        {
                            r.unh.Release match
                            {
                                case "04B" =>
                                    MSCONSMessage04B.phrase (rest) match
                                    {
                                        case MSCONSMessage04B.Success (message, rest) =>
                                            assert (rest.atEnd)
                                            check (message)
                                        case _ => fail ("parse failed")
                                    }
                                case _ => fail (s"${r.unh.Type} version ${r.unh.Version} release ${r.unh.Release} is not supported")
                            }
                        }

                    case ServiceSegmentParser.Failure (msg, _) =>
                        fail (s"parse failure: $msg")
                    case ServiceSegmentParser.Error (msg, _) =>
                        fail (s"parse error: $msg")
                }
            case message.Failure (msg, _) =>
                fail (s"parse failure: $msg")
            case message.Error (msg, _) =>
                fail (s"parse error: $msg")
        }
    }

    test ("BGM")
    {
        parseAndCheck (mscons,
            message =>
            {
                assert (message.bgm.documentMessageName.DocumentNameCode == "7")
                assert (message.bgm.documentMessageIdentification.DocumentIdentifier == "slevu14572840D")
                assert (message.bgm.messageFunctionCode == "9")
            }
        )
    }

    test ("DTM")
    {
        parseAndCheck (mscons,
            message =>
            {
                assert (message.dtm.functionCodeQualifier == "137")
                assert (message.dtm.text == "201912140000")
                assert (message.dtm.formatCode == "203")
                val calendar = message.dtm.getTime
                assert (calendar.get (Calendar.YEAR) == 2019)
                assert (calendar.get (Calendar.MONTH) == 12 - 1) // months are 0 to 11
                assert (calendar.get (Calendar.DAY_OF_MONTH) == 14)
                assert (calendar.get (Calendar.HOUR) == 0)
                assert (calendar.get (Calendar.MINUTE) == 0)
            }
        )
    }
}
