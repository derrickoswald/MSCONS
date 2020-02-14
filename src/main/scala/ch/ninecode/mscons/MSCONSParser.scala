package ch.ninecode.mscons

import java.nio.channels.FileChannel
import java.nio.file.FileSystems
import java.nio.file.StandardOpenOption

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
                    case ServiceSegmentParser.Success (r, _) =>
                        if (   (r.unh.Type == "MSCONS")
                            && (r.unh.Version == "D"))
                        {
                            r.unh.Release match
                            {
                                case "04B" => println (r)
                                case _ => log.error (s"${r.unh.Type} version ${r.unh.Version} release ${r.unh.Release} is not supported")
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

object MSCONSParser
{

}