package ch.ninecode.edifact

case class ServiceSegment
(
    tag: String,
    name: String,
    description: String,
    fields: List[Field],
    notes: String
)


