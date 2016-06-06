/** Entry point for standalone execution.
  *
  * @note It is recommended to run the metric as a compiler plugin,
  *       otherwise not all features of Scala are supported.
  *       Higher kinded types: [[https://issues.scala-lang.org/browse/SI-9421]].
  *       Package declaration: [[https://issues.scala-lang.org/browse/SI-6657]].
  *       Relative imports: [[https://issues.scala-lang.org/browse/SI-6393]].
  *       And probably more.
  */
@deprecated("It is recommended to run the metric as a compiler plugin.", "06-06-2016")
object ReflectionRunner {
  import scalaz.effect.IO, IO._
  import ExtraIO._
  import scala.reflect.runtime.universe
  import scala.tools.reflect.ToolBox

  val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()
  val metric = new Metric[universe.type](universe)

  def main(args: Array[String]): Unit = haskellMain(args).unsafePerformIO()

  def haskellMain(args: Array[String]): IO[Unit] = {

    import scalaz._, Scalaz._
    import play.api.libs.json._
    import ExtraString.encode_utf8
    import ExtraGraph._

    val files = List( // TODO: Add proper command line parser.
      "../examples/src/main/scala/Recursion.scala",
      "../examples/src/main/scala/Exceptions.scala",
      "../examples/src/main/scala/HigherOrder.scala"
    )

    val dotToPng = command_to_function("dot -Tpng") compose encode_utf8

    for {
      funcs <- files.traverse(process_file).map(_.join)
      _     <- putStrLn("Writing \"call_graph.png\"...")
      png   <- dotToPng(ExtraGraph.to_dot((funcs map metric.local_call_graph).suml))
      _     <- write_all("call_graph.png", png)
      _     <- putStrLn("Writing \"results.json\"...")
      _     <- write_all_utf8("results.json", Json.prettyPrint(JsArray(funcs map metric.to_json)))
    } yield ()
  }

  def process_file(filename: String): IO[metric.Functions] =
    for {
      _     <- putStrLn("Read file " + filename)
      text  <- read_all_utf8(filename)

      _     <- putStrLn("Parse...")
      tree  <- IO(tb.parse(text))

      _     <- putStrLn("Type check...")
      ttree <- IO(tb.typecheck(tree))

      _     <- putStrLn("Compute functionalness...")
      funcs <- IO(metric.collect_function_information(ttree))
    } yield funcs
}
