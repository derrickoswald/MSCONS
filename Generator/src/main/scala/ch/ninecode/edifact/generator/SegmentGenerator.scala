package ch.ninecode.edifact.generator

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

import scala.io.Source

class SegmentGenerator
{
//    ServiceSegment(UNB,INTERCHANGE HEADER,To identify an interchange.,List(Field(10,S001,SYNTAX IDENTIFIER,M,1,,1,List(Field(0,0001,Syntax identifier,M,,a4,,List()), Field(0,0002,Syntax version number,M,,an1,,List()), Field(0,0080,Service code list directory version number,C,,an..6,,List()), Field(0,0133,Character encoding, coded,C,,an..3,,List()))), Field(20,S002,INTERCHANGE SENDER,M,1,,2,List(Field(0,0004,Interchange sender identification,M,,an..35,,List()), Field(0,0007,Identification code qualifier,C,,an..4,,List()), Field(0,0008,Interchange sender internal identification,C,,an..35,,List()), Field(0,0042,Interchange sender internal sub-identification,C,,an..35,,List()))), Field(30,S003,INTERCHANGE RECIPIENT,M,1,,2,List(Field(0,0010,Interchange recipient identification,M,,an..35,,List()), Field(0,0007,Identification code qualifier,C,,an..4,,List()), Field(0,0014,Interchange recipient internal identification,C,,an..35,,List()), Field(0,0046,Interchange recipient internal sub-identification,C,,an..35,,List()))), Field(40,S004,DATE AND TIME OF PREPARATION,M,1,,,List(Field(0,0017,Date,M,,n8,,List()), Field(0,0019,Time,M,,n4,,List()))), Field(50,0020,INTERCHANGE CONTROL REFERENCE,M,1,an..14,2,List()), Field(60,S005,RECIPIENT REFERENCE/PASSWORD DETAILS,C,1,,,List(Field(0,0022,Recipient reference/password,M,,an..14,,List()), Field(0,0025,Recipient reference/password qualifier,C,,an2,,List()))), Field(70,0026,APPLICATION REFERENCE,C,1,an..14,,List()), Field(80,0029,PROCESSING PRIORITY CODE,C,1,a1,,List()), Field(90,0031,ACKNOWLEDGEMENT REQUEST,C,1,n1,,List()), Field(100,0032,INTERCHANGE AGREEMENT IDENTIFIER,C,1,an..35,,List()), Field(110,0035,TEST INDICATOR,C,1,n1,,List())),
//
//
//    NOTES:
//
//    1.  S001/0002, shall be '4' to indicate this version of the syntax.
//
//    2.  The combination of the values carried in data elements S002, S003 and 0020
//    shall be used to identify uniquely the interchange, for the purpose of
//    acknowledgement.)
//    CodeList(1,Syntax identifier,Coded identification of the agency controlling the syntax, and
//        of the character repertoire used in an interchange.,a4,List(Code(UNOA,UN/ECE level A,As defined in the basic code table of ISO 646 with the
//    exceptions of lower case letters, alternative graphic
//    character allocations and national or application-
//    oriented graphic character allocations.), Code(UNOB,UN/ECE level B,As defined in the basic code table of ISO 646 with the
//    exceptions of alternative graphic character allocations
//        and national or application-oriented graphic character
//    allocations.), Code(UNOC,UN/ECE level C,As defined in ISO/IEC 8859-1 : Information technology -
//    Part 1: Latin alphabet No. 1.), Code(UNOD,UN/ECE level D,As defined in ISO/IEC 8859-2 : Information technology -
//    Part 2: Latin alphabet No. 2.), Code(UNOE,UN/ECE level E,As defined in ISO/IEC 8859-5 : Information technology -
//    Part 5: Latin/Cyrillic alphabet.), Code(UNOF,UN/ECE level F,As defined in ISO 8859-7 : Information processing - Part
//    7: Latin/Greek alphabet.), Code(UNOG,UN/ECE level G,As defined in ISO/IEC 8859-3 : Information technology -
//    Part 3: Latin alphabet No. 3.), Code(UNOH,UN/ECE level H,As defined in ISO/IEC 8859-4 : Information technology -
//    Part 4: Latin alphabet No. 4.), Code(UNOI,UN/ECE level I,As defined in ISO/IEC 8859-6 : Information technology -
//    Part 6: Latin/Arabic alphabet.), Code(UNOJ,UN/ECE level J,As defined in ISO/IEC 8859-8 : Information technology -
//    Part 8: Latin/Hebrew alphabet.), Code(UNOK,UN/ECE level K,As defined in ISO/IEC 8859-9 : Information technology -
//    Part 9: Latin alphabet No. 5.), Code(UNOL,UN/ECE level L,As defined in ISO/IEC 8859-15 : Information technology -
//    Part 15: Latin alphabet No. 9.), Code(UNOW,UN/ECE level W,ISO 10646-1 octet with code extension technique to
//    support UTF-8 (UCS Transformation Format, 8 bit)
//    encoding.), Code(UNOX,UN/ECE level X,Code extension technique as defined by ISO 2022 utilising
//    the escape techniques in accordance with ISO 2375.), Code(UNOY,UN/ECE level Y,ISO 10646-1 octet without code extension technique.)))
//    CodeList(2379,Date or time or period format code                      [C],Code specifying the representation of a date, time or
//        period.,an..3,List(Code(2,DDMMYY,Calendar date: D = Day; M = Month; Y = Year.), Code(3,MMDDYY,Calendar date: M = Month; D = Day; Y = Year.), Code(4,DDMMCCYY,Calendar date C=Century; Y=Year; M=Month; D=Day.), Code(5,DDMMCCYYHHMM,Calendar date and time: C=Century; Y=Year; M=Month;
//    D=Day; H=Hour; M=Minute.), Code(6,CCYYMMB,Half-month: CC=century YY=year MM=month, B=1:first half
//    month, B=2:second half month.), Code(7,CCYYMMW,Week within a calendar month: CC=century YY=year
//    MM=month. W=1-5 first week to fifth week in a month.), Code(8,CCYYMMDDS,Shift within a calendar day: CC=century YY=year MM=month
//    DD=day S=1-9 shift in a day.), Code(9,CCYYMMDDPP,Time period within a calendar day: CC=century YY=year
//    MM=month DD=day PP=00-99 time period.), Code(10,CCYYMMDDTHHMM,Calendar date including time with minutes: C=Century;
//    Y=Year; M=Month; D=Day; T=Time designator; H=Hour;
//    M=Minutes.
//        The character [T] shall be used as time designator to
//    indicate the start of the representation of the time.
//        For example: 20010912T1433.), Code(101,YYMMDD,Calendar date: Y = Year; M = Month; D = Day.), Code(102,CCYYMMDD,Calendar date: C = Century ; Y = Year ; M = Month ; D =
//    Day.), Code(103,YYWWD,Calendar week day: Y = Year ; W = Week ; D = Day Week
//    number 01 is always first week of January Day number 1
//    is always Monday.), Code(104,MMWW-MMWW,A period of time specified by giving the start week of a
//    month followed by the end week of a month. Data is to be
//    transmitted as consecutive characters without hyphen.), Code(105,YYDDD,Calendar day: Y = Year ; D = Day January the first = Day
//    001 Always start numbering the days of the year from
//        January 1st through December 31st.), Code(106,MMDD,Day of a month: M = Month; D = Day.), Code(107,DDD,Day's number within a specific year: D = Day.), Code(108,WW,Week's number within a specific year: W = Week.), Code(109,MM,Month's number within a specific year: M = Month.), Code(110,DD,Day's number within is a specific month: D = Day.), Code(201,YYMMDDHHMM,Calendar date including time without seconds: Y = Year;
//    M = Month; D = Day; H = Hour; M = Minute.), Code(202,YYMMDDHHMMSS,Calendar date including time with seconds: Y = Year; M =
//    Month; D = Day; H = Hour; m = Minutes = Seconds.), Code(203,CCYYMMDDHHMM,Calendar date including time with minutes: C=Century;
//    Y=Year; M=Month; D=Day; H=Hour; M=Minutes.), Code(204,CCYYMMDDHHMMSS,Calendar date including time with seconds:
//    C=Century;Y=Year;
//    M=Month;D=Day;H=Hour;M=Minute;S=Second.), Code(205,CCYYMMDDHHMMZHHMM,Calendar date including time and time zone expressed in
//    hours and minutes.
//    ZHHMM = time zone given as offset from Coordinated
//    Universal Time (UTC).), Code(206,YYMMDDHHMMZHHMM,Calendar date including time without seconds, with Time
//    Zone:
//        Y = Year; M = Month; D = Day; H = Hour; M = Minute,
//    Z = leading plus/minus sign, HHMM = difference to UTC in
//    Hours and Minutes.), Code(207,YYMMDDHHMMSSZHHMM,Calendar date including time with seconds, with Time
//    Zone:
//        Y = Year; M = Month; D = Day; H = Hour; M = Minute, S =
//    Seconds
//    Z = leading plus/minus sign, HHMM = difference to UTC in
//    Hours and Minutes.), Code(208,CCYYMMDDHHMMSSZHHMM,Calendar date including time with seconds, with Time
//    Zone:
//        C = Century, Y = Year; M = Month; D = Day; H = Hour; M =
//    Minute, S = Seconds, Z = leading plus/minus sign, HHMM =
//    difference to UTC in Hours and Minutes.), Code(209,HHMMSSZHHMM,Time with seconds and with Time Zone:
//    H = Hour; M = Minute, S = Seconds, Z = leading
//    plus/minus sign, HHMM = difference to UTC in Hours and
//    Minutes.), Code(210,HHMMSSZHHMM-HHMMSSZHHMM,A period of time specified by giving the start time
//    followed by the end time (both expressed by hours,
//    minutes, seconds and time zone leading plus/minus sign
//        and time zone difference in hours and minutes).), Code(301,YYMMDDHHMMZZZ,See 201 + Z = Time zone.), Code(302,YYMMDDHHMMSSZZZ,See 202 + Z = Time zone.), Code(303,CCYYMMDDHHMMZZZ,See 203 plus Z=Time zone.), Code(304,CCYYMMDDHHMMSSZZZ,See 204 plus Z=Time zone.), Code(305,MMDDHHMM,Month, day, hours, minutes; M = Month; D = Day; H =
//    Hour; M = Minute.), Code(306,DDHHMM,Day, hours, minutes; D = Day; H = Hour; M = Minute.), Code(307,CCYYMMDDHHMMSSFFF,Calendar date including time with seconds and
//    milliseconds
//    C = century, Y = year, M = month, D = day, H = hour, M =
//    minute, S = second, F = millisecond.), Code(401,HHMM,Time without seconds: H = Hour; m = Minute.), Code(402,HHMMSS,Time with seconds: H = Hour; m = Minute; s = Seconds.), Code(404,HHMMSSZZZ,See 402 plus Z=Time zone.), Code(405,MMMMSS,Time without hours: m=minutes, s=seconds.), Code(406,ZHHMM,Offset from Coordinated Universal Time (UTC) where Z is
//    plus (+) or minus (-).), Code(501,HHMMHHMM,Time span without seconds: H = Hour; m = Minute;.), Code(502,HHMMSS-HHMMSS,A period of time specified by giving the start time
//    followed by the end time (both expressed by hours
//    minutes and seconds). Data is to be transmitted as
//    consecutive characters without hyphen.), Code(503,HHMMSSZZZ-HHMMSSZZZ,A period of time specified by giving the start time
//    followed by the end time (both expressed by hours
//    minutes, seconds and time zone). Data is to be
//    transmitted as consecutive characters without hyphen.), Code(600,CC,Century.), Code(601,YY,Calendar year: Y = Year.), Code(602,CCYY,Calendar year including century: C = Century; Y = Year.), Code(603,YYS,Semester in a calendar year: Y = Year; S = Semester.), Code(604,CCYYS,Semester in a calendar year: C = Century; Y = Year; S =
//    Semester.), Code(608,CCYYQ,Quarter in a calendar year: C = Century; Y = Year; Q =
//    Quarter.), Code(609,YYMM,Month within a calendar year: Y = Year; M = Month.), Code(610,CCYYMM,Month within a calendar year: CC = Century; Y = Year; M
//    = Month.), Code(613,YYMMA,To specify a ten-day period within a month of a year (A
//    = ten day period).), Code(614,CCYYMMA,To specify a ten-day period within a month of a year,
//    including century  (A = ten day period).), Code(615,YYWW,Week within a calendar year: Y = Year; W = Week 1st week
//    of January = week 01.), Code(616,CCYYWW,Week within a calendar year: CC = Century; Y = Year; W =
//    Week (1st week of January = week 01).), Code(701,YY-YY,A period of time specified by giving the start year
//    followed by the end year (both without century). Data is
//    to be transmitted as consecutive characters without
//    hyphen.), Code(702,CCYY-CCYY,A period of time specified by giving the start year
//    followed by the end year (both including century). Data
//    is to be transmitted as consecutive characters without
//    hyphen.), Code(703,YYS-YYS,A period of time specified by giving the start semester
//    of a year followed by the end semester of a year (both
//    not including century). Data is to be transmitted as
//    consecutive characters without hyphen.), Code(704,CCYYS-CCYYS,A period of time specified by giving the start semester
//    of a year followed by the end semester of a year (both
//    including century). Data is to be transmitted as
//    consecutive characters without hyphen.), Code(705,YYPYYP,Format of period to be given without hyphen (P = period
//    of 4 months).), Code(706,CCYYP-CCYYP,Format of period to be given without hyphen (P = period
//    of 4 months).), Code(707,YYQ-YYQ,A period of time specified by giving the start quarter
//    of a year followed by the end quarter of year (both not
//    including century). Data is to be transmitted as
//    consecutive characters without hyphen.), Code(708,CCYYQ-CCYYQ,A period of time specified by giving the start quarter
//    of a year followed by the end quarter of year (both
//    including century). Data is to be transmitted as
//    consecutive characters without hyphen.), Code(709,YYMM-YYMM,A period of time specified by giving the start month of
//    a year followed by the end month of a year (both not
//    including century). Data is to be transmitted as
//    consecutive characters without hyphen.), Code(710,CCYYMM-CCYYMM,A period of time specified by giving the start month of
//    a year followed by the end month of a year (both
//    including century). Data is to be transmitted as
//    consecutive characters without hyphen.), Code(713,YYMMDDHHMM-YYMMDDHHMM,A period of time specified by giving the start time
//    followed by the end time (format year, month, day, hour
//    and minute). Data is to be transmitted as consecutive
//    characters without hyphen.), Code(715,YYWW-YYWW,A period of time specified by giving the start week of a
//    year followed by the end week of year (both not
//    including century). Data is to be transmitted as
//    consecutive characters without hyphen.), Code(716,CCYYWW-CCYYWW,A period of time specified by giving the start week of a
//    year followed by the end week of year (both including
//    century). Data is to be transmitted as consecutive
//    characters without hyphen.), Code(717,YYMMDD-YYMMDD,A period of time specified by giving the start date
//    followed by the end date (both not including century).
//    Data is to be transmitted as consecutive characters
//    without hyphen.), Code(718,CCYYMMDD-CCYYMMDD,A period of time specified by giving the start date
//    followed by the end date (both including century). Data
//    is to be transmitted as consecutive characters without
//    hyphen.), Code(719,CCYYMMDDHHMM-CCYYMMDDHHMM,A period of time which includes the century, year,
//    month, day, hour and minute. Format of period to be
//        given in actual message without hyphen.), Code(720,DHHMM-DHHMM,Format of period to be given without hyphen (D=day of
//    the week, 1=Monday; 2=Tuesday; ... 7=Sunday).), Code(801,Year,To indicate a quantity of years.), Code(802,Month,To indicate a quantity of months.), Code(803,Week,To indicate a quantity of weeks.), Code(804,Day,To indicate a quantity of days.), Code(805,Hour,To indicate a quantity of hours.), Code(806,Minute,To indicate a quantity of minutes.), Code(807,Second,To indicate a quantity of seconds.), Code(808,Semester,To indicate a quantity of semesters (six months).), Code(809,Four months period,To indicate a quantity of four months periods.), Code(810,Trimester,To indicate a quantity of trimesters (three months).), Code(811,Half month,To indicate a quantity of half months.), Code(812,Ten days,To indicate a quantity of ten days periods.), Code(813,Day of the week,Numeric representation of the day (Monday = 1).), Code(814,Working days,Number of working days.)))

    lazy val segments: List[ServiceSegment] =
    {
        val source = Source.fromFile ("../ref/Ss40000.txt", "UTF-8")
        val text = source.getLines.mkString ("\n")
        source.close
        val parser = new ParseServiceSegmentList ()
        parser.parse (parser.list, text) match
        {
            case parser.Success (matched: List[ServiceSegment], _) => matched
            case _ => null
        }
    }

    lazy val codes: List[CodeList] =
    {
        val source = Source.fromFile ("../ref/sl40210.txt", "UTF-8")
        val text = source.getLines.mkString ("\n")
        source.close
        val parser = new ParseServiceCodeList ()
        parser.parse (parser.list, text) match
        {
            case parser.Success (matched: List[CodeList], _) => matched
            case _ => null
        }
    }

    lazy val codelist: List[CodeList] =
    {
        val source = Source.fromFile ("../ref/d17a/uncl/UNCL.17A", "UTF-8")
        val text = source.getLines.mkString ("\n")
        source.close
        val parser = new ParseCodeList ()
        parser.parse (parser.list, text) match
        {
            case parser.Success (matched: List[CodeList], _) => matched
            case _ => null
        }
    }

    /**
     * Generate the service segment parser.
     *
     * @param filename output file name
     */
    def generate (filename: String): Unit =
    {
        val s = new StringBuilder ()
        s.append (
            """package ch.ninecode.edifact
              |
              |import scala.util.parsing.combinator._
              |
              |class ServiceSegmentParser (una: UNA) extends Parsers
              |{
              |    type Elem = Segment
              |
              |""".stripMargin)
        for (segment <- segments)
        {
            val name = segment.name
            val description = segment.description.replace ("\n", "\n     * ")
            val tag = segment.tag
            s.append (
               s"""    /**
                  |     * $name
                  |     *
                  |     * $description
                  |     */
                  |    def $tag: Parser[Field] =
                  |    {
                  |""".stripMargin)
            for (field <- segment.fields)
            {
                for (subfield <- field.subfields)
                    s.append (
                       s"""        /**
                          |         * ${subfield.name}
                          |         */
                          |        def ${tag}_${subfield.tag}: Parser[Field] = ???
                          |
                          |""".stripMargin)
                val subfields = field.subfields.map (field => s"${tag}_${field.tag}${field.qualifier}").mkString (" ~ ")
                s.append (
                   s"""        /**
                      |         * ${field.name}
                      |         */
                      |        def ${tag}_${field.tag}: Parser[Field] = ${if ("" != subfields) subfields else "???"}
                      |
                      |""".stripMargin)
            }
            val fields = segment.fields.map (field => s"${tag}_${field.tag}${field.qualifier}").mkString (" ~ ")
            s.append (
               s"""        $fields
                  |    }
                  |
                  |""".stripMargin)
        }
        s.append (
            """}
              |""".stripMargin)
        Files.write (Paths.get (filename), s.toString.getBytes (StandardCharsets.UTF_8))
    }
}
