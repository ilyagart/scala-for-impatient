// Chapter 9. Files and Regular Expressions
import scala.io.Source.{fromInputStream, fromURL}
import scala.util.matching.Regex.Match
val stream = getClass.getClassLoader.getResourceAsStream("123.txt")
val url = getClass.getClassLoader.getResource("123.txt")
//noinspection SourceNotClosed
val iter = fromURL(url).getLines()
iter
  .mkString("\n")
//noinspection SourceNotClosed
iter.toArray

fromInputStream(stream).getLines().mkString("\n")
stream.close()

scala.util.matching.Regex
val numPattern = """\s+[0-9]+\s+""".r
//for (s <- numPattern.findAllIn("99 bottles, 98 bottles"))
//  println(s)

numPattern.replaceFirstIn("99 bottles, 98 bottles", "XX")
"XX bottles, 98 bottles"
numPattern.replaceAllIn("99 bottles, 98 bottles", "XX")
"XX bottles, XX bottles"
val vars = Map("x" -> "a var", "y" -> """some \$ and \ signs""")
val text = "A text with variables %x, %y and %z."
val varPattern = """%(\w+)""".r
val mapper = (m: Match) => vars get (m group 1)
val repl = varPattern replaceSomeIn (text, mapper)

val varPattern2 = """\$[0-9]+""".r
def format(message: String, vars: String*) =
  varPattern2.replaceSomeIn(message, m => vars.lift(m.matched.tail.toInt))
format(
  "At $1, there was $2 on $0.",
  "planet 7",
  "12:30 pm",
  "a disturbance of the force"
)

