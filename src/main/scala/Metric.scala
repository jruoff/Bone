// TODO: Aggregation, run on libraries
// TODO: Evaluation, survey
// TODO: Scope extension to traits, objects, etc.

import scala.reflect.api.Universe

/**
  * Created by jasper on 3-11-15.
  */
final class Metric[U <: Universe](val universe: U) {
  import ExtraString._
  import play.api.libs.json._
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



  def declaration_type(symbol: Symbol): Type = {
    assert(!is_anonymous(symbol))
    symbol.typeSignature
  }

  def declaration_type(application: Tree): Type = {
    assert(!is_anonymous(application))
    declaration_type(application.symbol)
  }

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



  def sum_json(left: JsValue, right: JsValue): JsValue = {
    // JsNumber(0) is left and right identity for all other JsValues.
    if (left  == JsNumber(0)) return right
    if (right == JsNumber(0)) return left

    (left, right) match {
      case (l: JsString, r: JsString) => JsArray (Seq(l, r))   // String + String
      case (l: JsString, JsArray (r)) => JsArray (Seq(l) ++ r) // String + Array
      case (JsArray (l), r: JsString) => JsArray (l ++ Seq(r)) // Array  + String
      case (JsArray (l), JsArray (r)) => JsArray (l ++ r)      // Array  + Array
      case (JsNumber(l), JsNumber(r)) => JsNumber(l +  r)      // Add numbers
      case (JsObject(l), JsObject(r)) =>
        import scalaz.syntax.std.map._
        JsObject(l.toMap.unionWith(r.toMap)(sum_json)) // Union sum JSON objects

      // Other combinations are not defined.
      case pair => throw new RuntimeException("sum_json not defined for " + pair.toString)
    }
  }

  /** Serialize the [[FunctionInformation]] object as a JSON object.
    *
    * @param f the function information.
    * @return A JSON object containing the serialization.
    */
  def to_json(f: FunctionInformation): JsObject = {

    /** Serialize a stream of named applications.
      *
      * @param named_applications The stream of named applications.
      * @return A JSON object containing a list of names, and a distinct list of names.
      */
    def total_distinct(named_applications: List[Tree]): JsValue =
      if (named_applications.isEmpty)
        JsNumber(0)
      else {
        val names = to_names(named_applications)
        Json.obj(
          "total"     -> names.size,
          "histogram" -> JsObject(names.groupBy(identity).mapValues(x => JsNumber(x.size)))
        )
      }

    /** Split and serialize a stream of named applications.
      *
      * There are two kinds of named applications, those that are declared implicit and those that are not.
      *
      * @param named_applications The stream of named applications.
      * @return A JSON object containing separate objects for explicit and implicit applications.
      */
    def split_named(named_applications: List[Tree]): JsValue =
      if (named_applications.isEmpty)
        JsNumber(0)
      else {
        val (implicit_, explicit) = named_applications partition is_implicit
        Json.obj(
          "explicit" -> total_distinct(explicit ),
          "implicit" -> total_distinct(implicit_)
        )
      }

    /** Split and serialize a stream of applications.
      *
      * There are two kinds of applications, those that are named, and those that are anonymous.
      *
      * @param applications The stream of applications.
      * @return A JSON object containing separate objects for named and anonymous applications.
      */
    def split_applications(applications: List[Tree]): JsValue =
      if (applications.isEmpty)
        JsNumber(0)
      else {
        val (anonymous, named) = applications partition is_anonymous
        Json.obj(
          "named"     -> split_named(named),
          "anonymous" -> anonymous.size
        )
      }

    def applications_type_predicate(applications: List[Tree])(predicate: Type => Boolean): JsValue = {
      val application = applications filter { tree =>                        predicate(application_type(tree)) }
      val declaration = applications filter { tree => !is_anonymous(tree) && predicate(declaration_type(tree)) }

      if (application.isEmpty && declaration.isEmpty)
        JsNumber(0)
      else
        Json.obj(
          "application" -> split_applications(application),
          "declaration" -> split_named(declaration)
        )
    }

    def type_analysis(serializer: (Type => Boolean) => JsValue,
                      serializer_kleene: (Type => Kleene) => JsValue
                     ): JsObject = Json.obj(
      "higher_order"      -> serializer(type_util.is_higher_order),
      "mutable_result"    -> serializer_kleene(type_util.has_mutable_result),
      "mutable_parameter" -> serializer_kleene(t => type_util.takes_mutable_parameter(t, Nil)),
      "monadic_result"    -> serializer(type_util.has_monadic_result),
      "unit_result"       -> serializer(type_util.has_unit_result)
    )

    Json.obj(
      "name"           -> f.name,
      "interface"      -> Json.obj(
        "type"           -> f.`type`.toString,
        "explicit_type"  -> type_util.strip_implicit_parameter_lists(f.`type`).toString,
        "result_type"    -> type_util.method_result_type            (f.`type`).toString,
        "type_analysis"  -> type_analysis(
          property => JsArray(if (property(f.`type`)               ) Seq(JsString(f.name)) else Seq()),
          property => JsArray(if (property(f.`type`) == Kleene.True) Seq(JsString(f.name)) else Seq())
        )
      ),
      "implementation" -> Json.obj(
        "size"           -> get_size(f.implementation),
        "self_recursive" -> is_self_recursive(f),
        "while_loops"    -> while_loops(f.implementation).size,
        "throws"         -> f.throws.map(_.toString),
        "vars"           -> var_definitions(f.implementation),
        "lambdas"        -> anonymous_functions(f.implementation).size,
        "applications"   -> split_applications(f.applications),
        "type_analysis"  -> type_analysis(
          applications_type_predicate(f.applications),
          kl => applications_type_predicate(f.applications)(t => kl(t).boolean(false))
        )
      )
    )
  }
}
