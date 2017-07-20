package ch.ninecode.mscons

import java.util.regex.Pattern

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
  def main (args: Array[String]) =
  {
      val up = new UNAParser ()
      up.parse ("UNA:+.? 'UNB+UNOC:3+12X-0000000858-F:500+") match
      {
          case up.Success (una, rest) =>
          {
              val message = new MessageParser (una)
              println (message.parse (rest))
          }
          case up.Failure (msg,_) => println ("FAILURE: " + msg)
          case up.Error (msg,_) => println ("ERROR: " + msg)
      }
  }
}