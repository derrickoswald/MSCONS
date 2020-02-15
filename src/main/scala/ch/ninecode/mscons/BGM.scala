package ch.ninecode.mscons

import ch.ninecode.edifact.FieldExtractor

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
    documentMessageName: Document_Message_Name = Document_Message_Name (),
    documentMessageIdentification: Document_Message_Identification = Document_Message_Identification (),
    messageFunctionCode: String = null,
    responseTypeCode: String = null)

object BGM extends FieldExtractor[BGM]
{
    //        010    C002 DOCUMENT/MESSAGE NAME                      C    1
    //        010    1001  Document name code                        C      an..3
    //        020    1131  Code list identification code             C      an..17
    //        030    3055  Code list responsible agency code         C      an..3
    //        040    1000  Document name                             C      an..35

    private lazy val c002_1001 = alphanumeric (3)
    private lazy val c002_1131 = alphanumeric (17)
    private lazy val c002_3055 = alphanumeric (3)
    private lazy val c002_1000 = alphanumeric (35)
    private lazy val c002 =
        subfields (
            c002_1001.? ~ c002_1131.? ~ c002_3055.? ~ c002_1000.? ^^
                { case c002_1001 ~ c002_1131 ~ c002_3055 ~ c002_1000  => Document_Message_Name (c002_1001.orNull, c002_1131.orNull, c002_3055.orNull, c002_1000.orNull) }
        )

    //    1004  Document identifier                       C      an..35
    //    1056  Version identifier                        C      an..9
    //    1060  Revision identifier                       C      an..6

    private lazy val c106_1004 = alphanumeric (35)
    private lazy val c106_1056 = alphanumeric (9)
    private lazy val c106_1060 = alphanumeric (6)
    private lazy val c106 =
        subfields (
            c106_1004.? ~ c106_1056.? ~ c106_1060.? ^^
                { case c106_1004 ~ c106_1056 ~ c106_1060 => Document_Message_Identification (c106_1004.orNull, c106_1056.orNull, c106_1060.orNull) }
        )

    //    030    1225 MESSAGE FUNCTION CODE                      C    1 an..3

    private lazy val _1225 = alphanumeric (3)

    //    040    4343 RESPONSE TYPE CODE                         C    1 an..3

    private lazy val _4343 = alphanumeric (3)

    lazy val bgm_fields: Parser[BGM] =
        fields (
            c002.? ~ c106.? ~ _1225.? ~ _4343.? ^^
                { case c002 ~ c106 ~ _1225 ~ _4343 => BGM (c002.orNull, c106.orNull, _1225.orNull, _4343.orNull) }
        )

    override def phrase: Parser[BGM] = bgm_fields
}
