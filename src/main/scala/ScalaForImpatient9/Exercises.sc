import java.io.{File, PrintWriter}
import scala.io.Source.fromURL

// 1
val url = getClass.getClassLoader.getResource("123.txt")
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

