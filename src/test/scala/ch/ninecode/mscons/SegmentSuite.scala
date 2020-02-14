package ch.ninecode.mscons

import org.scalatest.FunSuite
import ch.ninecode.edifact.Segment
import ch.ninecode.edifact.SegmentParser
import ch.ninecode.edifact.SegmentScanner
import ch.ninecode.edifact.ServiceSegmentParser

class SegmentSuite extends FunSuite
{
    test ("BGM")
    {
        val scanner = SegmentScanner ("UNB+UNOC:3+12X-SAK-N------6:500+12X-SAK-N------6:500+191215:0430+eslevu14572840++TL'UNH+slevu14572840D+MSCONS:D:04B:UN:2.2e'BGM+7+slevu14572840D+9'")
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
                                    val next = rest.first
                                    assert (next.name == "BGM")
                                    val bgm = BGM (next.fields)
                                    assert (bgm.c002.DocumentNameCode == "7")
                                    assert (bgm.c106.DocumentIdentifier == "slevu14572840D")
                                    assert (bgm._1225 == "9")
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
}
