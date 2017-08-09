package ch.ninecode.mscons

import java.util.regex.Pattern
import java.util.Properties
import org.slf4j.LoggerFactory

import scala.util.parsing.combinator._

class MessageParser (una: UNA) extends RegexParsers
{
    def word: Parser[String]   = """.*""".r       ^^ { _.toString }
    def message: Parser[String] = word ^^ { case unb => unb }
    def parse (in: Input): ParseResult[String] =
        message (in)
}

object MessageParser
{
    def main (args: Array[String]): Unit =
    {
        val properties =
        {
            val in = this.getClass.getResourceAsStream ("/app.properties")
            val p = new Properties ()
            p.load (in)
            in.close ()
            p
        }
        val APPLICATION_NAME = getClass.getName.substring (getClass.getName.lastIndexOf (".") + 1, getClass.getName.length - 1)
        val APPLICATION_VERSION = properties.getProperty ("version")
        //val SPARK = properties.getProperty ("spark")

        val log = LoggerFactory.getLogger (APPLICATION_NAME)
        log.info (APPLICATION_NAME + " v" + APPLICATION_VERSION)
        val up = new UNAParser ()
        up.parse ("UNA:+.? 'UNB+UNOC:3+12X-0000000858-F:500+") match
        {
            case up.Success (una: UNA, rest) =>
                val message = new MessageParser (una)
                log.info (message.parse (rest).toString)
            case up.Failure (msg, _) => log.error ("parse failure: " + msg.substring (0, 200))
            case up.Error (msg, _) => log.error ("parse error: " + msg.substring (0, 200))
        }
    }
}