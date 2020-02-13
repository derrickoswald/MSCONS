package ch.ninecode.edifact.generator

case class ServiceSegment
(
    tag: String,
    name: String,
    description: String,
    fields: List[Field],
    notes: String
)
