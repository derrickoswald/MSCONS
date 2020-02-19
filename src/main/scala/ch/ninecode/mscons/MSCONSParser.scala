package ch.ninecode.mscons

import java.nio.channels.FileChannel
import java.nio.file.FileSystems
import java.nio.file.StandardOpenOption
import java.text.SimpleDateFormat
import java.util.Calendar

import org.slf4j.LoggerFactory
import org.slf4j.Logger

import ch.ninecode.edifact.ServiceSegmentParser
import ch.ninecode.edifact.Segment
import ch.ninecode.edifact.SegmentParser
import ch.ninecode.edifact.SegmentScanner

case class MSCONSParser (options: MSCONSOptions)
{
    val log: Logger = LoggerFactory.getLogger (getClass)

    def parse (name: String): Unit =
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
                                            if (!rest.atEnd)
                                                log.warn (s"message incompletely parsed, stopped at ${rest.first}")
                                            val readings = message.getReadings
                                            val template = new SimpleDateFormat ("yyyy-MM-dd HH:mm:ss z")
                                            readings.map (x => println (s"${x._1} ${x._2} ${template.format (x._3.getTime)} ${x._4} ${x._5}+${x._6}j ${x._7}"))
                                        case MSCONSMessage04B.Failure (message, _) =>
                                            log.error (s"parse failed '$message'")
                                        case MSCONSMessage04B.Error (message, _) =>
                                            log.error (s"parse error '$message'")
                                    }
                                case _ =>
                                    log.error (s"${r.unh.Type} version ${r.unh.Version} release ${r.unh.Release} is not supported")
                            }
                        }

                    case ServiceSegmentParser.Failure (msg, _) =>
                        log.error (s"parse failure: $msg")
                    case ServiceSegmentParser.Error (msg, _) =>
                        log.error (s"parse error: $msg")
                }
            case message.Failure (msg, _) =>
                log.error (s"parse failure: $msg")
            case message.Error (msg, _) =>
                log.error (s"parse error: $msg")
        }
    }
}
