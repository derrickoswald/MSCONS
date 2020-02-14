package ch.ninecode.mscons

import ch.ninecode.edifact.Field
import ch.ninecode.edifact.FieldExtractor
import ch.ninecode.edifact.FieldListParser

//    010    C002 DOCUMENT/MESSAGE NAME                      C    1
//    1001  Document name code                        C      an..3
//    1131  Code list identification code             C      an..17
//    3055  Code list responsible agency code         C      an..3
//    1000  Document name                             C      an..35
//
//    020    C106 DOCUMENT/MESSAGE IDENTIFICATION            C    1
//    1004  Document identifier                       C      an..35
//    1056  Version identifier                        C      an..9
//    1060  Revision identifier                       C      an..6
//
//    030    1225 MESSAGE FUNCTION CODE                      C    1 an..3
//
//    040    4343 RESPONSE TYPE CODE                         C    1 an..3

/** DOCUMENT/MESSAGE NAME */
case class Document_Message_Name (
    DocumentNameCode: String = null,
    CodeListIdentificationCode: String = null,
    CodeListResponsibleAgencyCode: String = null,
    DocumentName: String = null
    )

/** DOCUMENT/MESSAGE IDENTIFICATION */
case class Document_Message_Identification (
    DocumentIdentifier: String = null,
    VersionIdentifier: String = null,
    RevisionIdentifier: String = null
    )

/** To indicate the type and function of a message and to transmit the identifying number. */
case class BGM (
   c002: Document_Message_Name = Document_Message_Name (),
   c106: Document_Message_Identification = Document_Message_Identification (),
   _1225: String = null,
   _4343: String = null)

object BGM extends FieldExtractor
{
    //        010    C002 DOCUMENT/MESSAGE NAME                      C    1
    //        010    1001  Document name code                        C      an..3
    //        020    1131  Code list identification code             C      an..17
    //        030    3055  Code list responsible agency code         C      an..3
    //        040    1000  Document name                             C      an..35

    def c002_1001: Parser[String] = alphanumeric (3)
    def c002_1131: Parser[String] = alphanumeric (17)
    def c002_3055: Parser[String] = alphanumeric (3)
    def c002_1000: Parser[String] = alphanumeric (35)
    type Q = Option[String] ~ Option[String] ~ Option[String] ~ Option[String]
    val c002_p: Parser[Q] = c002_1001.? ~ c002_1131.? ~ c002_3055.? ~ c002_1000.?
    def c002: Parser[Document_Message_Name] =
        fields (c002_p, (r: Q) => Document_Message_Name (r._1._1._1.orNull, r._1._1._2.orNull, r._1._2.orNull, r._2.orNull))

    //    1004  Document identifier                       C      an..35
    //    1056  Version identifier                        C      an..9
    //    1060  Revision identifier                       C      an..6

    def c106_1004: Parser[String] = alphanumeric (35)
    def c106_1056: Parser[String] = alphanumeric (9)
    def c106_1060: Parser[String] = alphanumeric (6)
    type R = Option[String] ~ Option[String] ~ Option[String]
    def c106_p: Parser[R] = c106_1004.? ~ c106_1056.? ~ c106_1060.?
    def c106: Parser[Document_Message_Identification] =
        fields (c106_p, (r: R) => Document_Message_Identification (r._1._1.orNull, r._1._2.orNull, r._2.orNull))

    //    030    1225 MESSAGE FUNCTION CODE                      C    1 an..3

    def _1225: Parser[String] = alphanumeric (3)

    //    040    4343 RESPONSE TYPE CODE                         C    1 an..3

    def _4343: Parser[String] = alphanumeric (3)

    type S = Option[Document_Message_Name] ~ Option[Document_Message_Identification] ~ Option[String] ~ Option[String]
    val bgm: Parser[S] = c002.? ~ c106.? ~ _1225.? ~ _4343.?
    def bgm_parser: Parser[BGM] =
        segments (bgm, (r: S) => BGM (r._1._1._1.orNull, r._1._1._2.orNull, r._1._2.orNull, r._2.orNull))
    // { case c002 ~ c106 ~ s1 ~ s2 => BGM (c002.orNull, c106.orNull, s1.orNull, s2.orNull) })

    def apply (fields: List[Field]): BGM =
    {
        bgm_parser (FieldListParser (fields)) match
        {
            case Success (r, _) => r
            case Failure (msg, _) =>
                log.error (s"BGM segment parse failure: $msg")
                BGM ()
            case Error (msg, _) =>
                log.error (s"service segments parse error: $msg")
                BGM ()
        }
    }
    // ^^ { case one ~ two => Member ("", one :: two :: Nil) }
}

