package ch.ninecode.mscons

import java.util.Calendar

import org.scalatest.FunSuite

import ch.ninecode.edifact.Segment
import ch.ninecode.edifact.SegmentListParser
import ch.ninecode.edifact.SegmentParser
import ch.ninecode.edifact.SegmentScanner
import ch.ninecode.edifact.ServiceSegmentParser

class SegmentSuite extends FunSuite
{
    abstract class Mock[T] extends MSCONSMessage
    {
        def parse(p: Parser[T], in: SegmentListParser): ParseResult[T] = p(in)
        def phrase: Parser[T]
        def parseAndCheck (text: String, check: T => Unit): Unit =
        {
            val scanner = SegmentScanner (text)
            val message = SegmentParser (scanner.una)
            val segments = message.segment.*
            segments.apply (scanner) match
            {
                case message.Success (result: List[Segment], remainder) =>
                    // result.foreach (segment => println (segment))
                    assert (remainder.atEnd)
                    val reader = SegmentListParser (result)
                    parse (phrase, reader) match
                    {
                        case Success (message, rest) =>
                            assert (rest.atEnd)
                            check (message)
                        case Failure (message, _) =>
                            fail (s"parse failed '$message'")
                        case Error (message, _) =>
                            fail (s"parse error '$message'")
                    }
                case message.Failure (message, _) =>
                    fail (s"segment parse failed '$message'")
                case message.Error (message, _) =>
                    fail (s"segment parse error '$message'")
            }
        }
    }

    def parseAndCheck (text: String, check: MSCONSMessage04B => Unit): Unit =
    {
        val scanner = SegmentScanner (text)
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
                                        case MSCONSMessage04B.Failure (message, _) =>
                                            fail (s"parse failed '$message'")
                                        case MSCONSMessage04B.Error (message, _) =>
                                            fail (s"parse error '$message'")
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
        new Mock[BGM] { def phrase: Parser[BGM] = MSCONSMessage04B.bgm.asInstanceOf[Parser[BGM]] }
            .parseAndCheck (
                "BGM+7+slevu14572840D+9'",
                bgm =>
                {
                    assert (bgm.documentMessageName.get.documentNameCode.contains ("7"))
                    assert (bgm.documentMessageIdentification.get.documentIdentifier.contains ("slevu14572840D"))
                    assert (bgm.messageFunctionCode.contains ("9"))
                    assert (bgm.responseTypeCode.isEmpty)
                }
            )
    }

    test ("DTM")
    {
        new Mock[DTM] { def phrase: Parser[DTM] = MSCONSMessage04B.dtm.asInstanceOf[Parser[DTM]] }
            .parseAndCheck (
                "DTM+137:201912140000:203'",
                dtm =>
                {
                    assert (dtm.functionCodeQualifier == "137")
                    assert (dtm.text.contains ("201912140000"))
                    assert (dtm.formatCode.contains ("203"))
                    val calendar = dtm.getTime
                    assert (calendar.get (Calendar.YEAR) == 2019)
                    assert (calendar.get (Calendar.MONTH) == 12 - 1) // months are 0 to 11
                    assert (calendar.get (Calendar.DAY_OF_MONTH) == 14)
                    assert (calendar.get (Calendar.HOUR) == 0)
                    assert (calendar.get (Calendar.MINUTE) == 0)
                }
            )
    }

    test ("Group1")
    {
        new Mock[Option[List[Group1]]] { def phrase: Parser[Option[List[Group1]]] = MSCONSMessage04B.group1.asInstanceOf[Parser[Option[List[Group1]]]] }
            .parseAndCheck (
                "RFF+Z13:13008'",
                group1 =>
                {
                    assert (group1.nonEmpty)
                    assert (group1.get.length == 1)
                    val item1 = group1.get.head
                    assert (item1.rff.referenceCodeQualifier.contains ("Z13"))
                    assert (item1.rff.referenceIdentifier.contains ("13008"))
                    assert (item1.rff.documentLineIdentifier.isEmpty)
                    assert (item1.rff.referenceVersionIdentifier.isEmpty)
                    assert (item1.rff.revisionIdentifier.isEmpty)
                    assert (item1.dtm_p.isEmpty)
                }
            )
    }

    test ("Message")
    {
        val mscons = "UNB+UNOC:3+12X-SAK-N------6:500+12X-SAK-N------6:500+191215:0430+eslevu14572840++TL'UNH+slevu14572840D+MSCONS:D:04B:UN:2.2e'BGM+7+slevu14572840D+9'DTM+137:201912140000:203'RFF+Z13:13008'"
        parseAndCheck (
            mscons,
            message =>
            {
                assert (message.bgm.documentMessageName.get.documentNameCode.contains ("7"))
                assert (message.bgm.documentMessageIdentification.get.documentIdentifier.contains ("slevu14572840D"))
                assert (message.bgm.messageFunctionCode.contains ("9"))
                assert (message.bgm.responseTypeCode.isEmpty)

                assert (message.dtm.functionCodeQualifier == "137")
                assert (message.dtm.text.contains ("201912140000"))
                assert (message.dtm.formatCode.contains ("203"))
                val calendar = message.dtm.getTime
                assert (calendar.get (Calendar.YEAR) == 2019)
                assert (calendar.get (Calendar.MONTH) == 12 - 1) // months are 0 to 11
                assert (calendar.get (Calendar.DAY_OF_MONTH) == 14)
                assert (calendar.get (Calendar.HOUR) == 0)
                assert (calendar.get (Calendar.MINUTE) == 0)

                assert (message.group1.nonEmpty)
                assert (message.group1.get.length == 1)
                val item1 = message.group1.get.head
                assert (item1.rff.referenceCodeQualifier.contains ("Z13"))
                assert (item1.rff.referenceIdentifier.contains ("13008"))
                assert (item1.rff.documentLineIdentifier.isEmpty)
                assert (item1.rff.referenceVersionIdentifier.isEmpty)
                assert (item1.rff.revisionIdentifier.isEmpty)
                assert (item1.dtm_p.isEmpty)
            }
        )
    }
}
