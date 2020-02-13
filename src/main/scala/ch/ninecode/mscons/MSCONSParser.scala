package ch.ninecode.mscons

import java.nio.channels.FileChannel
import java.nio.file.FileSystems
import java.nio.file.StandardOpenOption
import java.util.Properties

import org.slf4j.LoggerFactory
import org.slf4j.Logger

import ch.ninecode.edifact.ServiceSegmentParser
import ch.ninecode.edifact.Segment
import ch.ninecode.edifact.SegmentListParser
import ch.ninecode.edifact.SegmentParser
import ch.ninecode.edifact.SegmentScanner

object MSCONSParser
{
    def main (args: Array[String]): Unit =
    {
        val log: Logger = LoggerFactory.getLogger (getClass)
        val properties =
        {
            val in = this.getClass.getResourceAsStream ("/app.properties")
            val p = new Properties ()
            p.load (in)
            in.close ()
            p
        }
        val APPLICATION_NAME: String = properties.getProperty ("artifactId")
        val APPLICATION_VERSION: String = properties.getProperty ("version")
        //val SPARK: String = properties.getProperty ("spark")

        new MSCONSOptionsParser (APPLICATION_NAME, APPLICATION_VERSION).parse (args, MSCONSOptions ()) match
        {
            case Some (options) =>
                if (options.valid)
                {
//                    if (options.verbose) org.apache.log4j.LogManager.getLogger (getClass.getName).setLevel (org.apache.log4j.Level.INFO)

                    if (options.mscons.nonEmpty)
                    {
                        for (name <- options.mscons)
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
                    else
                        log.error ("no input MSCONS files specified")
                }
                if (!options.unittest)
                    sys.exit (0)
            case None =>
                sys.exit (1)
        }
    }
}