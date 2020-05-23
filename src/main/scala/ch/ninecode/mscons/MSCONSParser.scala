package ch.ninecode.mscons

import java.nio.channels.FileChannel
import java.nio.file.FileSystems
import java.nio.file.StandardOpenOption
import java.util.Calendar

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import ch.ninecode.edifact.Segment
import ch.ninecode.edifact.SegmentParser
import ch.ninecode.edifact.SegmentScanner
import ch.ninecode.edifact.ServiceSegmentParser

case class MSCONSParser (options: MSCONSOptions)
{
    type ID = String
    type Quantity = String
    type Time = Calendar
    type Period = Int
    type Real = Double
    type Imaginary = Double
    type Units = String

    val log: Logger = LoggerFactory.getLogger (getClass)

    def failure (msg: String): List[(ID, Quantity, Time, Period, Real, Imaginary, Units)] =
    {
        log.error (msg)
        List ()
    }

    def parse (name: String): List[(ID, Quantity, Time, Period, Real, Imaginary, Units)] =
    {
        val path = FileSystems.getDefault.getPath (name)
        val file = FileChannel.open (path, StandardOpenOption.READ)
        val buffer = file.map (FileChannel.MapMode.READ_ONLY, 0L, file.size ())
        file.close ()

            val scanner = SegmentScanner (buffer)
            val message = SegmentParser (scanner.una)
            val segments = message.segment.*
            segments.apply (scanner) match
            {
                case message.Success (result: List[Segment], _) =>
                    val x = ServiceSegmentParser.read (result)
                    x match
                    {
                        case ServiceSegmentParser.Success (r, rest) =>
                            if ((r.unh.Type == "MSCONS")
                                && (r.unh.Version == "D"))
                            {
                                r.unh.Release match
                                {
                                    case "04B" =>
                                        MSCONSMessage04B.phrase (rest) match
                                        {
                                            case MSCONSMessage04B.Success (message, rest) =>
                                                if (!rest.atEnd)
                                                    log.warn (s"message incompletely parsed, stopped at ${rest.first}")
                                                message.getReadings
                                            case MSCONSMessage04B.Failure (message, _) =>
                                                failure (s"parse failed '$message'")
                                            case MSCONSMessage04B.Error (message, _) =>
                                                failure (s"parse error '$message'")
                                        }
                                    case _ =>
                                        failure (s"${r.unh.Type} version ${r.unh.Version} release ${r.unh.Release} is not supported")
                                }
                            }
                            else
                                failure (s"unsupported message type ${r.unh.Type} version ${r.unh.Version}")

                        case ServiceSegmentParser.Failure (msg, _) =>
                            failure (s"service segment parse failure: $msg")
                        case ServiceSegmentParser.Error (msg, _) =>
                            failure (s"service segment parse error: $msg")
                    }
                case message.Failure (msg, _) =>
                    failure (s"segment parse failure: $msg")
                case message.Error (msg, _) =>
                    failure (s"segment parse error: $msg")
            }
        }
}
