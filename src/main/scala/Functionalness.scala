object Functionalness
{
  case class Histogram(total: Int, histogram: Map[String, Int])

  case class SplitNamed(explicit: Histogram, `implicit`: Histogram)

  case class SplitApplications(named: SplitNamed, anonymous: Int)

  case class FilteredApplications(application: SplitApplications, declaration: SplitNamed)

  case class DeclarationTypeAnalysis(
                                      higher_order: Set[String],
                                      mutable_result: Set[String],
                                      mutable_parameter: Set[String],
                                      monadic_result: Set[String],
                                      unit_result: Set[String]
                                    )

  case class ApplicationTypeAnalysis(
                                      higher_order: FilteredApplications,
                                      mutable_result: FilteredApplications,
                                      mutable_parameter: FilteredApplications,
                                      monadic_result: FilteredApplications,
                                      unit_result: FilteredApplications
                                    )

  case class Interface(`type`: String, explicit_type: String, result_type: String, type_analysis: DeclarationTypeAnalysis)

  case class Implementation(
                             size          : Int,
                             self_recursive: Int,
                             while_loops   : Int,
                             throws        : Set[String],
                             vars          : List[String],
                             lambdas       : Int,
                             applications  : SplitApplications,
                             type_analysis : ApplicationTypeAnalysis
                           )

  case class FunctionAnalysis(names: Set[String], interface: Interface, implementation: Implementation)

  case class SourceAnalysis(source: Set[String], analysis: List[FunctionAnalysis])

  def merge(left: Histogram, right: Histogram): Histogram = {
    import scalaz.syntax.std.map._
    Histogram(left.total + right.total, left.histogram.unionWith(right.histogram)(_ + _))
  }

  def merge(left: SplitNamed, right: SplitNamed): SplitNamed =
    SplitNamed(merge(left.explicit, right.explicit), merge(left.`implicit`, right.`implicit`))

  def merge(left: SplitApplications, right: SplitApplications): SplitApplications =
    SplitApplications(merge(left.named, right.named), left.anonymous + right.anonymous)

  def merge(left: FilteredApplications, right: FilteredApplications): FilteredApplications =
    FilteredApplications(merge(left.application, right.application), merge(left.declaration, right.declaration))

  def merge(left: DeclarationTypeAnalysis, right: DeclarationTypeAnalysis): DeclarationTypeAnalysis =
    DeclarationTypeAnalysis(
      left.higher_order      ++ right.higher_order,
      left.mutable_result    ++ right.mutable_result,
      left.mutable_parameter ++ right.mutable_parameter,
      left.monadic_result    ++ right.monadic_result,
      left.unit_result       ++ right.unit_result
    )

  def merge(left: ApplicationTypeAnalysis, right: ApplicationTypeAnalysis): ApplicationTypeAnalysis =
    ApplicationTypeAnalysis(
      merge(left.higher_order     , right.higher_order),
      merge(left.mutable_result   , right.mutable_result),
      merge(left.mutable_parameter, right.mutable_parameter),
      merge(left.monadic_result   , right.monadic_result),
      merge(left.unit_result      , right.unit_result)
    )

  def merge(left: Interface, right: Interface): Interface =
    Interface("<multiple>", "<multiple>", "<multiple>", merge(left.type_analysis, right.type_analysis))

  def merge(left: Implementation, right: Implementation): Implementation =
    Implementation(
      left.size + right.size,
      left.self_recursive + right.self_recursive,
      left.while_loops + right.while_loops,
      left.throws ++ right.throws,
      left.vars ++ right.vars,
      left.lambdas + right.lambdas,
      merge(left.applications, right.applications),
      merge(left.type_analysis, right.type_analysis)
    )

  def merge(left: FunctionAnalysis, right: FunctionAnalysis): FunctionAnalysis =
    FunctionAnalysis(
      left.names ++ right.names,
      merge(left.interface, right.interface),
      merge(left.implementation, right.implementation)
    )

  def merge(left: SourceAnalysis, right: SourceAnalysis): SourceAnalysis =
    SourceAnalysis(left.source ++ right.source, left.analysis ++ right.analysis)

  def flatten(analysis: SourceAnalysis): SourceAnalysis =
    SourceAnalysis(analysis.source, List(analysis.analysis.reduceLeft(merge))) // TODO: Use a fold.

  case class TypeAnalysisSummary(
                                  total: Double,
                                  higher_order: Double,
                                  mutable_result: Double,
                                  mutable_parameter: Double,
                                  monadic_result: Double,
                                  unit_result: Double
                                )

  case class ImplementationSummary(
                                    size: Double,
                                    loops: Double,
                                    vars: Double,
                                    lambdas: Double
                                  )

  case class Summary(
                      files: Int,
                      interface: TypeAnalysisSummary,
                      self_recursive: Double,
                      implementation: ImplementationSummary,
                      application: TypeAnalysisSummary
                    )

  /**
    *
    * @param analysis A flattened analysis.
    * @return
    */
  def summary(analysis: SourceAnalysis): Summary = {
    assert(analysis.analysis.size == 1, "SourceAnalysis has to be flattened first!")

    val function       = analysis.analysis.head
    val interface      = function.interface     .type_analysis
    val implementation = function.implementation
    val application    = implementation.type_analysis

    val t1: Double = function.names.size
    val t2: Double = implementation.size
    val t3: Double = implementation.applications.named.explicit.total

    Summary(
      analysis.source.size,
      TypeAnalysisSummary(
        t1,
        interface.higher_order     .size / t1,
        interface.mutable_result   .size / t1,
        interface.mutable_parameter.size / t1,
        interface.monadic_result   .size / t1,
        interface.unit_result      .size / t1
      ),
      function.implementation.self_recursive / t1,
      ImplementationSummary(
        t2,
        implementation.while_loops / t2,
        implementation.vars.size   / t2,
        implementation.lambdas     / t2
      ),
      TypeAnalysisSummary(
        t3,
        application.higher_order     .declaration.explicit.total / t3,
        application.mutable_result   .declaration.explicit.total / t3,
        application.mutable_parameter.declaration.explicit.total / t3,
        application.monadic_result   .declaration.explicit.total / t3,
        application.unit_result      .declaration.explicit.total / t3
      )
    )
  }

  import play.api.libs.json.Json.format

  implicit val formatHistogram            = format[Histogram]
  implicit val formatSplitNamed           = format[SplitNamed]
  implicit val formatSplitApplications    = format[SplitApplications]
  implicit val formatFilteredApplications = format[FilteredApplications]
  implicit val formatTypeAnalysis1        = format[DeclarationTypeAnalysis]
  implicit val formatTypeAnalysis2        = format[ApplicationTypeAnalysis]
  implicit val formatInterface            = format[Interface]
  implicit val formatImplementation       = format[Implementation]
  implicit val formatFunctionAnalysis     = format[FunctionAnalysis]
  implicit val formatSourceAnalysis       = format[SourceAnalysis]

  implicit val formatTypeAnalysisSummary   = format[TypeAnalysisSummary]
  implicit val formatImplementationSummary = format[ImplementationSummary]
  implicit val formatSummary               = format[Summary]
}
