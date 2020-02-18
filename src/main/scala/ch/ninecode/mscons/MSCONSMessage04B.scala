package ch.ninecode.mscons

//    Pos     Tag Name                                     S   R
//
//    HEADER SECTION
//
//    00010   UNH Message header                           M   1
//    00020   BGM Beginning of message                     M   1
//    00030   DTM Date/time/period                         M   9
//    00040   CUX Currencies                               C   9
//
//    00050       ---- Segment group 1  ------------------ C   9----------------+
//    00060   RFF Reference                                M   1                |
//    00070   DTM Date/time/period                         C   9----------------+
//
//    00080       ---- Segment group 2  ------------------ C   99---------------+
//    00090   NAD Name and address                         M   1                |
//                                                                              |
//    00100       ---- Segment group 3  ------------------ C   9---------------+|
//    00110   RFF Reference                                M   1               ||
//    00120   DTM Date/time/period                         C   9---------------+|
//                                                                              |
//    00130       ---- Segment group 4  ------------------ C   9---------------+|
//    00140   CTA Contact information                      M   1               ||
//    00150   COM Communication contact                    C   9---------------++
//
//    DETAIL SECTION
//
//    00160   UNS Section control                          M   1
//
//    00170       ---- Segment group 5  ------------------ M   99999------------+
//    00180   NAD Name and address                         M   1                |
//                                                                              |
//    00190       ---- Segment group 6  ------------------ M   99999-----------+|
//    00200   LOC Place/location identification            M   1               ||
//    00210   DTM Date/time/period                         C   9               ||
//                                                                             ||
//    00220       ---- Segment group 7  ------------------ C   99-------------+||
//    00230   RFF Reference                                M   1              |||
//    00240   DTM Date/time/period                         C   9--------------+||
//                                                                             ||
//    00250       ---- Segment group 8  ------------------ C   99-------------+||
//    00260   CCI Characteristic/class id                  M   1              |||
//    00270   DTM Date/time/period                         C   99-------------+||
//                                                                             ||
//    00280       ---- Segment group 9  ------------------ C   99999----------+||
//    00290   LIN Line item                                M   1              |||
//    00300   PIA Additional product id                    C   9              |||
//    00310   IMD Item description                         C   9              |||
//    00320   PRI Price details                            C   9              |||
//    00330   NAD Name and address                         C   9              |||
//    00340   MOA Monetary amount                          C   9              |||
//                                                                            |||
//    00350       ---- Segment group 10 ------------------ M   9999----------+|||
//    00360   QTY Quantity                                 M   1             ||||
//    00370   DTM Date/time/period                         C   9             ||||
//    00380   STS Status                                   C   9-------------+|||
//                                                                            |||
//    00390       ---- Segment group 11 ------------------ C   99------------+|||
//    00400   CCI Characteristic/class id                  M   1             ||||
//    00410   MEA Measurements                             C   99            ||||
//    00420   DTM Date/time/period                         C   9-------------++++
//
//    SUMMARY SECTION
//
//    00430   CNT Control total                            C   99
//    00440   UNT Message trailer                          M   1

case class Group1 (
    rff: RFF,
    dtm_p: Option[List[DTM]])

case class Group3 (
    rff: RFF,
    dtm_p: Option[List[DTM]]
)

case class Group4 (
    cta: CTA,
    com: Option[List[COM]])

case class Group2 (
    nad: NAD,
    group3: Option[List[Group3]],
    group4: Option[List[Group4]]
)

case class Group7 (
    reference: RFF,
    dateTimePeriod: Option[List[DTM]]
)

case class Group8 (
    characteristicClassId: CCI,
    dateTimePeriod: Option[List[DTM]]
)

case class Group10 (
    qty: QTY,
    dtm: Option[List[DTM]],
    sts: Option[List[STS]]
)

case class Group11 (
    cci: CCI,
    mea: Option[List[MEA]],
    DTM: Option[List[DTM]]
)

case class Group9 (
    lin: LIN,
    pia: Option[List[PIA]],
//    imd: Option[List[IMD]],
//    pri: Option[List[PRI]],
//    nad: Option[List[NAD]],
//    moa: Option[List[MOA]],
    group10: List[Group10],
    group11: Option[List[Group11]]
)

case class Group6 (
    loc: LOC,
    dtm: Option[List[DTM]],
    group7: Option[List[Group7]],
    group8: Option[List[Group8]],
    group9: Option[List[Group9]]
)

case class Group5 (
    nad: NAD,
    group6: List[Group6]
)

case class MSCONSMessage04B (
    bgm: BGM,
    dtm: DTM,
    cux: Option[CUX],
    group1: Option[List[Group1]],
    group2: Option[List[Group2]],
    uns: UNS,
    group5: List[Group5]
)

object MSCONSMessage04B extends MSCONSMessage
{
    lazy val bgm: Parser[BGM] = expect ("BGM", x => BGM (x))
    lazy val dtm: Parser[DTM] = expect ("DTM", x => DTM (x))
    lazy val dtms: Parser[Option[List[DTM]]] = repAtMostN (9, false, dtm).? ^^
        (g => if (g.isDefined && 0 < g.get.length) Some (g.get) else None)
    lazy val cux: Parser[Option[CUX]] = expect ("CUX", x => CUX (x)).?
    lazy val rff: Parser[RFF] = expect ("RFF", x => RFF (x))
    lazy val group1: Parser[Option[List[Group1]]] = repAtMostN (9, false, rff ~ dtms).? ^^
        (g => if (g.isDefined && 0 < g.get.length) Some (g.get.map ({ case rff ~ dtms => Group1 (rff, dtms) })) else None)

    lazy val nad: Parser[NAD] = expect ("NAD", x => NAD (x))
    lazy val group3: Parser[Option[List[Group3]]] = repAtMostN (9, false, rff ~ dtms).? ^^
        (g => if (g.isDefined && 0 < g.get.length) Some (g.get.map ({ case rff ~ dtms => Group3 (rff, dtms) })) else None)

    lazy val cta: Parser[CTA] = expect ("CTA", x => CTA (x))
    lazy val com: Parser[COM] = expect ("COM", x => COM (x))
    lazy val coms: Parser[Option[List[COM]]] = repAtMostN (9, false, com).? ^^
        (g => if (g.isDefined && 0 < g.get.length) Some (g.get) else None)
    lazy val group4: Parser[Option[List[Group4]]] = repAtMostN (9, false, cta ~ coms).? ^^
        (g => if (g.isDefined && 0 < g.get.length) Some (g.get.map ({ case cta ~ coms => Group4 (cta, coms) })) else None)

    lazy val group2: Parser[Option[List[Group2]]] = repAtMostN (99, false, nad ~ group3 ~ group4).? ^^
        (g => if (g.isDefined && 0 < g.get.length) Some (g.get.map ({ case nad ~ g3 ~ g4 => Group2 (nad, g3, g4) })) else None)

    lazy val group7: Parser[Option[List[Group7]]] = repAtMostN (99, false, rff ~ dtms).? ^^
        (g => if (g.isDefined && 0 < g.get.length) Some (g.get.map ({ case rff ~ dtms => Group7 (rff, dtms) })) else None)

    lazy val group8: Parser[Option[List[Group8]]] = repAtMostN (99, false, cci ~ dtms).? ^^
        (g => if (g.isDefined && 0 < g.get.length) Some (g.get.map ({ case cci ~ dtms => Group8 (cci, dtms) })) else None)

    lazy val qty: Parser[QTY] = expect ("QTY", x => QTY (x))
    lazy val sts: Parser[STS] = expect ("STS", x => STS (x))
    lazy val stss: Parser[Option[List[STS]]] = repAtMostN (9, false, sts).? ^^
        (g => if (g.isDefined && 0 < g.get.length) Some (g.get) else None)
    lazy val group10: Parser[List[Group10]] = repAtMostN (9999, true, qty ~ dtms ~ stss) ^^
        (g => g.map ({ case qty ~ dtms ~ stss => Group10 (qty, dtms, stss) }))

    lazy val cci: Parser[CCI] = expect ("CCI", x => CCI (x))
    lazy val mea: Parser[MEA] = expect ("MEA", x => MEA (x))
    lazy val meas: Parser[Option[List[MEA]]] = repAtMostN (99, false, mea).? ^^
        (g => if (g.isDefined && 0 < g.get.length) Some (g.get) else None)
    lazy val group11: Parser[Option[List[Group11]]] = repAtMostN (99, false, cci ~ meas ~ dtms).? ^^
        (g => if (g.isDefined && 0 < g.get.length) Some (g.get.map ({ case cci ~ meas ~ dtms => Group11 (cci, meas, dtms) })) else None)

    lazy val lin: Parser[LIN] = expect ("LIN", x => LIN (x))
    lazy val pia: Parser[PIA] = expect ("PIA", x => PIA (x))
    lazy val pias: Parser[Option[List[PIA]]] = repAtMostN (9, false, pia).? ^^
        (g => if (g.isDefined && 0 < g.get.length) Some (g.get) else None)
    lazy val group9: Parser[Option[List[Group9]]] = repAtMostN (99999, false, lin ~ pias ~ /* ... */ group10 ~ group11 ).? ^^
        (g => if (g.isDefined && 0 < g.get.length) Some (g.get.map ({ case lin ~ pias ~ group10 ~ group11 => Group9 (lin, pias, group10, group11) })) else None)

    lazy val loc: Parser[LOC] = expect ("LOC", x => LOC (x))
    lazy val group6: Parser[List[Group6]] = repAtMostN (99999, true, loc ~ dtms ~ group7 ~ group8 ~ group9 ) ^^
        (g => g.map ({ case loc ~ dtms ~ group7 ~ group8 ~ group9 => Group6 (loc, dtms, group7, group8, group9) }))

    lazy val group5: Parser[List[Group5]] = repAtMostN (99999, true, nad ~ group6 ) ^^
        (g => g.map ({ case nad ~ group6 => Group5 (nad, group6) }))

    lazy val uns: Parser[UNS] = expect ("UNS", x => UNS (x))

    val phrase: Parser[MSCONSMessage04B] = bgm ~ dtm ~ cux ~ group1 ~ group2 ~ uns ~ group5 ^^
        { case bgm ~ dtm ~ cux ~ group1 ~ group2 ~ uns ~ group5 => MSCONSMessage04B (bgm, dtm, cux, group1, group2, uns, group5 ) }
}
