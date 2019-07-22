package ch.ninecode.edifact

/**
 * Name and contents of a single segment.
 *
 * @param name the three uppercase character name of the segment
 * @param contents the contents of the segment, exclusive of the name and segment terminator
 */
case class Segment (name: String, contents: String)
