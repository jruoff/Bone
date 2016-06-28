object Functionalness
{
  import scalaz.Monoid
  import scalaz.Scalaz._

  case class Histogram(total: Int, histogram: Map[String, Int])

  implicit val HistogramMonoid: Monoid[Histogram] = new Monoid[Histogram] {
    import scalaz.syntax.std.map._

    def append(a: Histogram, b: => Histogram): Histogram =
      Histogram(a.total + b.total, a.histogram.unionWith(b.histogram)(_ + _))

    def zero: Histogram = Histogram(0, Map())
  }

  case class SplitNamed(explicit: Histogram, `implicit`: Histogram)

  implicit val SplitNamedMonoid: Monoid[SplitNamed] = new Monoid[SplitNamed] {
    def append(a: SplitNamed, b: => SplitNamed): SplitNamed =
      SplitNamed(a.explicit |+| b.explicit, a.`implicit` |+| b.`implicit`)

    def zero: SplitNamed = SplitNamed(Monoid[Histogram].zero, Monoid[Histogram].zero)
  }

  case class SplitApplications(named: SplitNamed, anonymous: Int)

  implicit val SplitApplicationsMonoid: Monoid[SplitApplications] = new Monoid[SplitApplications] {
    def append(a: SplitApplications, b: => SplitApplications): SplitApplications =
      SplitApplications(a.named |+| b.named, a.anonymous + b.anonymous)

    def zero: SplitApplications = SplitApplications(Monoid[SplitNamed].zero, 0)
  }

  case class FilteredApplications(application: SplitApplications, declaration: SplitNamed)

  implicit val FilteredApplicationsMonoid: Monoid[FilteredApplications] = new Monoid[FilteredApplications] {
    def append(a: FilteredApplications, b: => FilteredApplications): FilteredApplications =
      FilteredApplications(a.application |+| b.application, a.declaration |+| b.declaration)

    def zero: FilteredApplications = FilteredApplications(Monoid[SplitApplications].zero, Monoid[SplitNamed].zero)
  }

  case class DeclarationTypeAnalysis(
                                      higher_order: Set[String],
                                      mutable_result: Set[String],
                                      mutable_parameter: Set[String],
                                      monadic_result: Set[String],
                                      unit_result: Set[String]
                                    )

  implicit val DeclarationTypeAnalysisMonoid: Monoid[DeclarationTypeAnalysis] = new Monoid[DeclarationTypeAnalysis] {
    def append(a: DeclarationTypeAnalysis, b: => DeclarationTypeAnalysis): DeclarationTypeAnalysis =
      DeclarationTypeAnalysis(
        a.higher_order      ++ b.higher_order,
        a.mutable_result    ++ b.mutable_result,
        a.mutable_parameter ++ b.mutable_parameter,
        a.monadic_result    ++ b.monadic_result,
        a.unit_result       ++ b.unit_result
      )

    def zero: DeclarationTypeAnalysis = DeclarationTypeAnalysis(Set(), Set(), Set(), Set(), Set())
  }

  case class ApplicationTypeAnalysis(
                                      higher_order: FilteredApplications,
                                      mutable_result: FilteredApplications,
                                      mutable_parameter: FilteredApplications,
                                      monadic_result: FilteredApplications,
                                      unit_result: FilteredApplications
                                    )

  implicit val ApplicationTypeAnalysisMonoid: Monoid[ApplicationTypeAnalysis] = new Monoid[ApplicationTypeAnalysis] {
    def append(a: ApplicationTypeAnalysis, b: => ApplicationTypeAnalysis): ApplicationTypeAnalysis =
      ApplicationTypeAnalysis(
        a.higher_order      |+| b.higher_order,
        a.mutable_result    |+| b.mutable_result,
        a.mutable_parameter |+| b.mutable_parameter,
        a.monadic_result    |+| b.monadic_result,
        a.unit_result       |+| b.unit_result
      )

    def zero: ApplicationTypeAnalysis = ApplicationTypeAnalysis(
      Monoid[FilteredApplications].zero,
      Monoid[FilteredApplications].zero,
      Monoid[FilteredApplications].zero,
      Monoid[FilteredApplications].zero,
      Monoid[FilteredApplications].zero
    )
  }

  case class Interface(`type`: String, explicit_type: String, result_type: String, type_analysis: DeclarationTypeAnalysis)

  implicit val InterfaceMonoid: Monoid[Interface] = new Monoid[Interface] {
    def append(a: Interface, b: => Interface): Interface =
      Interface("<multiple>", "<multiple>", "<multiple>", a.type_analysis |+| b.type_analysis)

    def zero: Interface = Interface("<none>", "<none>", "<none>", Monoid[DeclarationTypeAnalysis].zero)
  }

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

  implicit val ImplementationMonoid: Monoid[Implementation] = new Monoid[Implementation] {
    def append(a: Implementation, b: => Implementation): Implementation =
      Implementation(
        a.size           +  b.size,
        a.self_recursive +  b.self_recursive,
        a.while_loops    +  b.while_loops,
        a.throws        ++  b.throws,
        a.vars          ++  b.vars,
        a.lambdas        +  b.lambdas,
        a.applications  |+| b.applications,
        a.type_analysis |+| b.type_analysis
      )

    def zero: Implementation = Implementation(
      0, 0, 0, Set(), List(), 0,
      Monoid[SplitApplications].zero,
      Monoid[ApplicationTypeAnalysis].zero
    )
  }

  case class FunctionAnalysis(names: Set[String], interface: Interface, implementation: Implementation)

  implicit val FunctionAnalysisMonoid: Monoid[FunctionAnalysis] = new Monoid[FunctionAnalysis] {
    def append(a: FunctionAnalysis, b: => FunctionAnalysis): FunctionAnalysis =
      FunctionAnalysis(
        a.names          ++  b.names,
        a.interface      |+| b.interface,
        a.implementation |+| b.implementation
      )

    def zero: FunctionAnalysis = FunctionAnalysis(Set(), Monoid[Interface].zero, Monoid[Implementation].zero)
  }

  case class SourceAnalysis(source: Set[String], analysis: List[FunctionAnalysis])

  implicit val SourceAnalysisMonoid: Monoid[SourceAnalysis] = new Monoid[SourceAnalysis] {
    def append(a: SourceAnalysis, b: => SourceAnalysis): SourceAnalysis =
      SourceAnalysis(a.source ++ b.source, a.analysis ++ b.analysis)

    def zero: SourceAnalysis = SourceAnalysis(Set(), List())
  }

  def flatten(analysis: SourceAnalysis): SourceAnalysis =
    SourceAnalysis(analysis.source, List(analysis.analysis.suml))

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
