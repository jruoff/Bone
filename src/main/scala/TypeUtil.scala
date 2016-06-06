import scala.annotation.tailrec
import scala.reflect.api.Universe

/**
  * Created by jasper on 26-4-16.
  */
final class TypeUtil[U <: Universe](val universe: U) {
  import universe._

  /** Get the overloads of a specific method on a particular type.
    *
    * @param t The type of which the method should be a member.
    * @param name The name of the method.
    * @return All overloads of this method on the specified type.
    */
  def method_overloads(t: Type, name: String): Traversable[Symbol] =
    for (member <- t.members if member.isMethod && member.name == TermName(name))
      yield member

  /** Check whether a type has a particular method.
    *
    * @param t The type of which the method should be a member.
    * @param name The name of the method.
    * @return True iff the specified type has a method with the specified name.
    */
  def has_method(t: Type, name: String): Boolean = method_overloads(t, name).nonEmpty

  /** Check whether it is possible to use the given type in a Scala for-expression.
    *
    * @param t The type to use in the Scala for-expression.
    * @return True iff the specified type can be used in a Scala for-expression.
    * @note The current implementation is very basic. It does not take into account any
    *       implicit conversions, and does not check whether the expression would be well typed.
    */
  def monad_syntax(t: Type) = has_method(t, "flatMap") && has_method(t, "map")
  // methods(t).map(s => s.name.toString + ": " + s.typeSignature.toString).mkString("\n")

  /** A list of all function type symbols Function0 to Function22. */
  private val functionTypeSymbols = List(
    typeOf[() => Int].typeSymbol,
    typeOf[(Int) => Int].typeSymbol,
    typeOf[(Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int].typeSymbol
  )

  /** Test whether a type is a function type.
    *
    * @param t The type.
    * @return True iff the type is a function type.
    */
  def is_function_type(t: Type) = t match {
    case TypeRef(_, sym, _) => functionTypeSymbols.contains(sym)
    case _ => false
  }

  /** A list of all tuple type symbols Tuple1 to Tuple22. */
  private val tupleTypeSymbols = List(
    typeOf[(Int)].typeSymbol,
    typeOf[(Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)].typeSymbol,
    typeOf[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)].typeSymbol
  )

  /** Test whether a type is a tuple type.
    *
    * @param t The type.
    * @return True iff the type is a tuple type.
    */
  def is_tuple_type(t: Type) = t match {
    case TypeRef(_, sym, _) => tupleTypeSymbols.contains(sym)
    case _ => false
  }

  /** Get the order of a type.
    *
    * @param t The type.
    * @return The order of the given type.
    * {{{
    *    0 -> No function types.
    *    1 -> First order function.
    * >= 2 -> Higher order function.
    * }}}
    * @note This function takes into account the type parameters of non-function types.
    *       That is, the type {{{List[T => U]}}} will be classified as 1th order.
    *       This function counts Scala currying as well.
    *       This function includes (possibly curried) implicit arguments.
    */
  def order(t: Type): Int = t.dealias match {
    case PolyType(typeParams, resultType) => order(resultType)

    case NullaryMethodType(result_type) =>
      order(result_type) // No explicit application needed, so not counted.

    case MethodType(parameter_symbols, result_type) =>
      (result_type :: parameter_symbols.map(_.typeSignature)).map(order).max + 1

    case TypeRef(pre, sym, args) =>
      util.Try(args.map(order).max).getOrElse(0) +
        (if (functionTypeSymbols.contains(sym)) 1 else 0)

    case _ => 0
  }

  /** Strip all implicit parameters from a type.
    *
    * @param t The type.
    * @return The new type without implicit parameters, all parameter lists that have become
    *         empty because of removal of the implicit parameters, are removed as well.
    */
  def strip_implicit_parameter_lists(t: Type): Type = t match {
    case PolyType(parameters, result_type) =>
      internal.polyType(parameters, strip_implicit_parameter_lists(result_type))
    case NullaryMethodType(result_type) =>
      internal.nullaryMethodType(strip_implicit_parameter_lists(result_type))
    case MethodType(parameters, result_type) => {
      val new_parameters = parameters.filterNot(_.isImplicit)
      val new_result_type = strip_implicit_parameter_lists(result_type)
      if (parameters.nonEmpty && new_parameters.isEmpty)
        new_result_type
      else
        internal.methodType(new_parameters, new_result_type)
    }
    case other @ _ => other
  }

  /** Merge all parameter lists of a method type. (Un-curry)
    *
    * @param t The method type.
    * @return The un-curried method type.
    */
  def merge_parameter_lists(t: Type): Type = t match {
    case PolyType(parameters, result_type) =>
      internal.polyType(parameters, merge_parameter_lists(result_type)) // TODO: Remove unused type params?
    // TODO: Can a poly type happen in between?
    case MethodType(parameters1, MethodType(parameters2, result_type)) =>
      merge_parameter_lists(internal.methodType(parameters1 ::: parameters2, result_type))
    case other @ _ => other
  }

  /** Get the result type of a (possibly polymorphic) method type.
    *
    * @param t The type of the method.
    * @return The type of the result of the method (the final result in case the method is curried).
    * @example The result type of the method type {{{[A, B](f: A => B)(l: List[A]): List[B]}}}
    *          is {{{List[B]}}}.
    * @note This function works only for method types, not for function types!
    */
  @tailrec
  def method_result_type(t: Type): Type = t match {
    case PolyType(parameters, result_type) => method_result_type(result_type)
    case NullaryMethodType(result_type) => method_result_type(result_type)
    case MethodType(parameters, result_type) => method_result_type(result_type)
    case other @ _ => other
  }

  /** Turn a method type into a function type.
    *
    * @param t The type of the method.
    * @return The function type corresponding to the method type.
    * @note As functions in Scala cannot be polymorphic,
    *       all type parameters are removed from the type.
    */
  def decay_method_type_to_function_type(t: Type): Type = t match {
    case PolyType(parameters, result_type) => decay_method_type_to_function_type(result_type)
    case NullaryMethodType(result_type) => // No need for explicit application
      decay_method_type_to_function_type(result_type)
    case MethodType(parameters, result_type) =>
      internal.typeRef(
        NoPrefix, functionTypeSymbols(parameters.size),
        parameters.map(_.typeSignature) :+ decay_method_type_to_function_type(result_type)
      )
    case other @ _ => other
  }

  /** Test whether a type is higher order.
    *
    * @param data_type The type.
    * @return True iff the type is a higher order method or function type.
    * @note This function ignores implicit method parameters.
    */
  def is_higher_order(data_type: Type): Boolean = order(strip_implicit_parameter_lists(data_type)) > 1

  /** Check whether a type is mutable.
    *
    * @param data_type_aliased A (possibly aliased) data type.
    * @param immutable_symbols A list of type symbols assumed to be immutable. (Can be used to pass
    *                          type parameters of polymorphic methods.)
    * @return Whether this given type is mutable.
    */
  def is_mutable(data_type_aliased: Type, immutable_symbols: List[Symbol] = Nil): Kleene = {
    import Kleene._

    assert(data_type_aliased != null)
    assert(data_type_aliased != NoType)

    val data_type = data_type_aliased.dealias

    // Some common types
    if (data_type =:= typeOf[Unit   ]) return False
    if (data_type =:= typeOf[Int    ]) return False
    if (data_type =:= typeOf[Boolean]) return False
    if (data_type =:= typeOf[String ]) return False

    // TODO: How does variance play a role in this?
    // Mutability of type arguments
    val mutable_type_argument = data_type match {
      case TypeRef(_, _, args) => args.foldLeft(False)((k, t) => k || is_mutable(t, immutable_symbols))
      case _ => False
    }

    // Types with a symbol
    val symbol = data_type.typeSymbol
    if (symbol != null && symbol != NoSymbol) {

      // Array type
      if (symbol == typeOf[Array[Int]].typeSymbol) return True

      // Immutable type symbol
      if (immutable_symbols.contains(symbol))
        return mutable_type_argument

      // Function or tuple type
      if (is_function_type(data_type) || is_tuple_type(data_type))
        return mutable_type_argument

      // Types with an owner
      val owner = symbol.owner
      if (owner != null || owner != NoSymbol) {

        // Known owners
        val   mutable_packages = List("scala.collection.mutable")
        val immutable_packages = List("scala.collection.immutable")

        if (  mutable_packages.contains(owner.fullName)) return True
        if (immutable_packages.contains(owner.fullName)) return mutable_type_argument
        return Unknown || mutable_type_argument
      }
    }

    Unknown
  }

  /** Check whether a method takes one or more mutable parameters.
    *
    * @param method_type The type of the method.
    * @param type_parameters The type parameters in scope. They are assumed to be immutable.
    * @return Whether the given method type takes one or more mutable parameters.
    * @note this function ignores implicit parameters.
    * @example TODO
    *
    * TODO: Instead of passing type parameters long it is also possible to specialize the type
    * TODO: by substituting all type parameters with for example Int.
    *
    * TODO: Make sure it works for lazy parameters: {{{foo(x: => Int): Int}}}
    */
  def takes_mutable_parameter(method_type: Type, type_parameters: List[Symbol] = Nil): Kleene =
    strip_implicit_parameter_lists(method_type) match {
      case PolyType(parameters, result_type) =>
        takes_mutable_parameter(result_type, parameters ::: type_parameters)

      case MethodType(parameters, result_type) =>
        parameters.foldLeft(Kleene.False)((k, p) => k || is_mutable(p.typeSignature, type_parameters)) ||
        takes_mutable_parameter(result_type, type_parameters)

      case _ => Kleene.False // Not a parameter
    }

  /** Get a list of the type parameter symbols of a method type.
    *
    * @param method_type The method type.
    * @return All symbols of type parameters. (Nil if the method is not polymorphic.)
    */
  def type_parameters(method_type: Type): List[Symbol] = method_type match {
    case PolyType(parameters, result_type) => type_parameters(result_type) ::: parameters
    // TODO: Double check poly type cannot happen in between!
    // case NullaryMethodType(result_type) => type_parameters(result_type)
    // case MethodType(parameters, result_type) => type_parameters(result_type)
    case _ => Nil
  }

  /** Check for a monadic result type.
    *
    * @param method_type The method type.
    * @return Whether it is possible to use the result of the given method type in a Scala for-expression.
    */
  def has_monadic_result(method_type: Type): Boolean =
    monad_syntax(method_result_type(method_type))

  /** Check whether a method type has Unit result type.
    *
    * @param method_type The method type.
    * @return Whether the method type has Unit result type.
    */
  def has_unit_result(method_type: Type): Boolean =
    method_result_type(method_type) =:= typeOf[Unit]

  /** Check wheter a mathod type has a mutable result type.
    *
    * @param method_type The method type.
    * @return True if the result type is probably mutable.
    *         False if the result type is probably immutable.
    *         Unknown if the mutability checker was unable to deduce.
    */
  def has_mutable_result(method_type: Type): Kleene =
    is_mutable(method_result_type(method_type), type_parameters(method_type))
}
