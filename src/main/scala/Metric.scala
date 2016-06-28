// TODO: Aggregation, run on libraries
// TODO: Evaluation, survey
// TODO: Scope extension to traits, objects, etc.

import scala.reflect.api.Universe

/**
  * Created by jasper on 3-11-15.
  */
final class Metric[U <: Universe](val universe: U) {
  import ExtraString._
  import universe._

  val type_util = new TypeUtil[universe.type](universe)

  def collect_function_information(t: Tree): Functions =
    for (d @ DefDef(_, _, _, _, _, _) <- t)
      yield FunctionInformation(d)

  /**
    * Class containing information the metric has collected about a function.
    */
  final case class FunctionInformation(definition: DefDef) {
    // mods: Modifiers
    // name: TermName
    // tparams: List[TypeDef]
    // vparamss: List[List[ValDef]]
    // tpt: Tree
    // rhs: Tree

    val symbol: Symbol = definition.symbol

    // The full name of this function.
    val name: String = full_name_simplified(symbol)

    // The type of this function.
    val `type`: Type = definition.symbol.typeSignature

    // The implementation of this function.
    val implementation: Tree = definition.rhs

    // Set of all types this function might throw.
    val throws: Set[Type] =
      (for (Throw(expression) <- implementation)
        yield expression.tpe).toSet

    // Trees of function applications.
    val applications: List[Tree] =
      for (Apply(function, _) <- implementation)
        yield function
  }



  def to_names(trees: List[Tree]): List[String] =
    trees map full_name_simplified



  type Functions = List[FunctionInformation]
  def Functions(xs: FunctionInformation*) = List(xs: _*) // *cries*

  /** Checks whether this function is self recursive.
    *
    * @return True iff this function contains a named call to itself.
    */
  def is_self_recursive(f: FunctionInformation): Boolean =
    f.applications filterNot is_anonymous map { _.symbol } contains f.symbol

  def full_name_simplified(symbol: Symbol): String =
    drop_prefix(symbol.fullName, "<expression-owner>.")

  def full_name_simplified(application: Tree): String =
    full_name_simplified(application.symbol)



  def is_anonymous(symbol: Symbol): Boolean =
    symbol == null || symbol == NoSymbol

  def is_anonymous(application: Tree): Boolean =
    is_anonymous(application.symbol)

  def is_implicit(application: Tree): Boolean = {
    assert(!is_anonymous(application))
    application.symbol.isImplicit
  }


  /** Get the declaration type of a symbol. */
  def declaration_type(symbol: Symbol): Type = {
    assert(!is_anonymous(symbol))
    symbol.typeSignature
  }

  /** Get the declaration type of an application.
    *
    * @note The declaration type is the type as defined by the declaration
    *       of the method that is being applied.
    */
  def declaration_type(application: Tree): Type = {
    assert(!is_anonymous(application))
    declaration_type(application.symbol)
  }

  /** Get the application type of an application.
    *
    * @note The application type is the type as defined by the the application
    *       of the method at the site of application. Any formal type parameters
    *       have been substituted for the local type arguments.
    */
  def application_type(application: Tree): Type = {
    assert(application.tpe != null)
    assert(application.tpe != NoType)
    application.tpe
  }



  def while_loops(implementation: Tree): List[LabelDef] =
    for (
      loop @ LabelDef(
        TermName(name1), Nil,
        If(
          _,
          Block(_, Apply(Ident(TermName(name2)), Nil)),
          Literal(Constant(()))
        )
      ) <- implementation if name1 == name2
    ) yield loop

  def anonymous_functions(implementation: Tree): List[String] =
    for (func @ Function(_, _) <- implementation)
      yield full_name_simplified(func.symbol)

  def var_definitions(implementation: Tree): List[String] =
    for (
      ValDef(Modifiers(Flag.MUTABLE, _, _), TermName(name), _, _)
      <- implementation
    ) yield name

  def get_size(implementation: Tree): Int = {
    var i = 0
    for (_ <- implementation)
      i = i + 1
    i
  }


  import ExtraGraph._

  def local_call_graph(f: FunctionInformation): CallGraph =
    ExtraGraph.local_call_graph(f.name, to_names(f.applications filterNot is_anonymous).toSet[String])


  import Functionalness._

  /** Serialize a stream of named applications.
    *
    * @param values The stream of named applications.
    * @return A JSON object containing a list of names, and a distinct list of names.
    */
  def make_histogram(values: List[String]): Histogram =
    Histogram(values.size, values groupBy identity mapValues (_.size))

  /** Split and serialize a stream of named applications.
    *
    * There are two kinds of named applications, those that are declared implicit and those that are not.
    *
    * @param named_applications The stream of named applications.
    * @return A JSON object containing separate objects for explicit and implicit applications.
    */
  def split_named(named_applications: List[Tree]): SplitNamed = {
    val (implicit_, explicit) = named_applications partition is_implicit
    SplitNamed(make_histogram(to_names(explicit)), make_histogram(to_names(implicit_)))
  }

  /** Split and serialize a stream of applications.
    *
    * There are two kinds of applications, those that are named, and those that are anonymous.
    *
    * @param applications The stream of applications.
    * @return A JSON object containing separate objects for named and anonymous applications.
    */
  def split_applications(applications: List[Tree]): SplitApplications = {
    val (anonymous, named) = applications partition is_anonymous
    SplitApplications(split_named(named), anonymous.size)
  }

  def declaration_serializer(f: FunctionInformation, property: Type => Boolean): Set[String] =
    if (property(f.`type`)) Set(f.name) else Set()

  def declaration_type_analysis(f: FunctionInformation): DeclarationTypeAnalysis =
    DeclarationTypeAnalysis(
      declaration_serializer(f,      type_util.is_higher_order),
      declaration_serializer(f, t => type_util   has_mutable_result    t boolean false),
      declaration_serializer(f, t => type_util takes_mutable_parameter t boolean false),
      declaration_serializer(f,      type_util.has_monadic_result),
      declaration_serializer(f,      type_util.has_unit_result)
    )

  def application_serializer(applications: List[Tree], predicate: Type => Boolean): FilteredApplications = {
    val application = applications filter { tree =>                        predicate(application_type(tree)) }
    val declaration = applications filter { tree => !is_anonymous(tree) && predicate(declaration_type(tree)) }
    FilteredApplications(split_applications(application), split_named(declaration))
  }

  def application_type_analysis(applications: List[Tree]): ApplicationTypeAnalysis =
    ApplicationTypeAnalysis(
      application_serializer(applications,      type_util.is_higher_order),
      application_serializer(applications, t => type_util   has_mutable_result    t boolean false),
      application_serializer(applications, t => type_util takes_mutable_parameter t boolean false),
      application_serializer(applications,      type_util.has_monadic_result),
      application_serializer(applications,      type_util.has_unit_result)
    )

  def interface_analysis(f: FunctionInformation): Interface =
    Interface(
      f.`type`.toString,
      type_util.strip_implicit_parameter_lists(f.`type`).toString,
      type_util.method_result_type            (f.`type`).toString,
      declaration_type_analysis(f)
    )

  def implementation_analysis(f: FunctionInformation): Implementation =
    Implementation(
      get_size(f.implementation),
      if (is_self_recursive(f)) 1 else 0,
      while_loops(f.implementation).size,
      f.throws.map(_.toString),
      var_definitions(f.implementation),
      anonymous_functions(f.implementation).size,
      split_applications(f.applications),
      application_type_analysis(f.applications)
    )

  def function_analysis(f: FunctionInformation): FunctionAnalysis =
    FunctionAnalysis(Set(f.name), interface_analysis(f), implementation_analysis(f))

  def source_analysis(source: String, functions: Functions): SourceAnalysis =
    Functionalness.SourceAnalysis(Set(source), functions map function_analysis)
}
