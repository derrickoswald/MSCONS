package ch.ninecode.edifact

import scala.util.parsing.input.Position
import scala.util.parsing.input.Reader

case class FieldScanner (segment: Segment, una: UNA, begin: Int = 0) extends Reader[Field]
{
    var start: Int = begin
    var end: Int = start

    override def first: Field =
    {
        var skip = false
        var stop = false
        var c = 0
        var intervals = List[(Int,Int)] () // start and size of each piece of the segment

        while (end < segment.contents.length && !stop)
        {
            c = segment.contents.charAt (end)
            end += 1
            if (skip)
            {
                if (c == una.data_element_separator)
                {
                    intervals = intervals :+ (start, end - 2)
                    start = start + end - 1
                    end = start
                }
                skip = false
            }
            else
            if (c == una.data_element_separator)
                stop = true
            else
                if (c == una.release_character)
                    skip = true
        }

        intervals = intervals :+ (start, if (stop) end - 1 else end)

        val string = intervals.map (
            item => segment.contents.substring (item._1, item._2)
        ).mkString ("")
        Field (string, List[Field] ())
    }

    override def rest: Reader[Field] = FieldScanner (segment: Segment, una: UNA, end)

    override def pos: Position = new Position { val line: Int = 0; val column: Int = begin; val lineContents: String = segment.contents }

    override def atEnd: Boolean = end >= segment.contents.length
}
