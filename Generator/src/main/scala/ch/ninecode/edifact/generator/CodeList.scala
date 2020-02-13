package ch.ninecode.edifact.generator

case class CodeList
(
    number: Int,
    title: String,
    description: String,
    representation: String,
    items: List[Code]
)
