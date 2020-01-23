import java.io.{File, FileOutputStream, ObjectOutputStream, PrintWriter}

import scala.io.Source
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

// 1
val source = fromURL(url)
source.getLines().toArray.reverse foreach println
source.close

// 2
def replaceTabsWithSpaces(s: String) =
  s.map(c => if (c == '\t') ' ' * 4 else c)
val source2 = fromURL(getClass.getClassLoader.getResource("tabs.txt"))
source2.getLines().toArray.map(replaceTabsWithSpaces)
source2.close

// 3
//noinspection SourceNotClosed
fromURL(getClass.getClassLoader.getResource("longwords.txt"))
  .getLines()
  .toArray
  .mkString
  .split("\\s+")
  .filter(_.length > 12)

// 4
val source25 = fromURL(getClass.getClassLoader.getResource("floatpoint.txt"))
val (count, sum, min, max): (Int, Float, Float, Float) = source25.mkString
  .split("[^0-9\\.-]+")
  .map(_.toFloat)
  .foldLeft((0, 0f, Float.MaxValue, Float.MinValue)) {
    case ((count, sum, min, max), elt) =>
      (
        count + 1,
        sum + elt,
        if (elt < min) elt else min,
        if (elt > max) elt else max
      )
  }

println(s"sum=$sum, avg=${sum / count}, min=$min, max=$max")

// 5
val printWriter = new PrintWriter("temp/floats.txt")

printWriter.write("num | %10s | %10s%n".format("power", "reciprocal"))
printWriter.write("=============================\n")
(0 to 20).foldLeft((1, 1.0)) {
  case ((exp, rec), i) =>
    printWriter.write("%3d | %10d | %10f%n".format(i, exp, rec))
    (exp * 2, rec / 2)
}
printWriter.close()

// 9
def walkFiles(rootDir: File): Iterator[File] = {
  val children = rootDir.listFiles
  children.filter(_.isFile).iterator ++
    children.filter(_.isDirectory).flatMap(walkFiles)
}
def countClassFiles(rootDir: File) =
  walkFiles(rootDir).count(_.getName.endsWith(".class"))
val path = ""
countClassFiles(new File(path))

