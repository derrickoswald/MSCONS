package ch.ninecode.edifact

case class Field
(
    position: Int,
    tag: String,
    name: String,
    status: String,
    repetition: String,
    representation: String,
    notes: String,
    subfields: List[Field]
)
{
    lazy val qualifier: String = if (status == "C") ".?" else ""
}
