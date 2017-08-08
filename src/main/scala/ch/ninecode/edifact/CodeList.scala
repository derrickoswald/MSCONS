package ch.ninecode.edifact

case class CodeList
(
   number: Int,
   title: String,
   description: String,
   representation: String,
   items: List[Code]
)

