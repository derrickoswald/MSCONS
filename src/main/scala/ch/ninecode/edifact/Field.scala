package ch.ninecode.edifact

case class Field (text: String, submembers: List[Field])
{
    override def toString: String = text
}
