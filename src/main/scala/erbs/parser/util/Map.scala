package erbs.parser.util

object EmptySet { def unapply(s: Set[String]): Boolean = s.isEmpty }
