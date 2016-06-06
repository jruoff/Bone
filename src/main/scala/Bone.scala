import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import java.nio.file.Paths

/** Scala compiler plugin entry point. */
class Bone(val global: Global) extends Plugin {
  val name = "bone"
  val description = "A tool to measure functionalness"
  val components = List[PluginComponent](Component)

  private object Component /* TODO: Rename */ extends PluginComponent {
    val global = Bone.this.global
    val phaseName = Bone.this.name
    val runsAfter = List("typer")

    def newPhase(prev: Phase) = new BonePhase(prev)

    class BonePhase(prev: Phase) extends StdPhase(prev) {

      override def name = Bone.this.name

      /** Make an absolute path relative to the current working directory.
        *
        * @param absolute_path String containing the absolute path to be made relative.
        * @return String containing the relative path corresponding to the absolute path.
        * @note Will return the original path in case of failure.
        */
      private def make_relative(absolute_path: String): String =
        try {
          Paths.get(System.getProperty("user.dir")).relativize(Paths.get(absolute_path)).toString
        } catch {
          case _: IllegalArgumentException => absolute_path
        }

      /** Apply compiler phase. (This function is called by the Scala compiler.)
        */
      def apply(unit: global.CompilationUnit): Unit = {
        import play.api.libs.json._
        import ExtraString._
        import ExtraIO._
        //import ExtraGraph._

        val relative_source_path = make_relative(unit.source.file.path)
        val relative_metric_path = Paths.get("bone").resolve(drop_suffix(relative_source_path, ".scala")).toString

        global.reporter.echo("Bone " + quote(relative_source_path))

        //val dotToImage = command_to_function("dot -Tpng") compose encode_utf8
        /*(dot: String) => for {
          image  <- command_to_function("dot -Tps")(encode_utf8(dot))
          zipped <- command_to_function("gzip -f")(image)
        } yield zipped*/

        val metric = new Metric[global.type](global) // Because we all love path-dependent types. Not.
        val funcs  = metric.collect_function_information(unit.body)
        val json   = Json.obj(
          "source"   -> relative_source_path,
          "analysis" -> JsArray(funcs map metric.to_json)
        )
        (for {
          //image <- dotToImage(ExtraGraph.to_dot((funcs map metric.local_call_graph).suml))
          _     <- make_directories(relative_metric_path)
          //_     <- write_all(relative_metric_path + ".png", image)
          _     <- write_all_utf8(relative_metric_path + ".json", Json.prettyPrint(json))
        } yield ()).unsafePerformIO()
      }
    }
  }
}
