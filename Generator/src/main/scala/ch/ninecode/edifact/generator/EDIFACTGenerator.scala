package ch.ninecode.edifact.generator

object EDIFACTGenerator
{
    def main (args: Array[String]): Unit =
    {
        val filename =
            if (0 == args.length)
                "target/ServiceSegmentParser.scala"
            else
                args(0)
        val generator = new SegmentGenerator
        generator.generate (filename)
        println (s"$filename generated")
    }
}
