/** CallGraph */
object ExtraGraph
{
  import ExtraString._
  import scalax.collection.Graph
  import scalax.collection.GraphEdge.DiEdge
  import scalaz.Monoid

  /** The type of graph to represent the call graph. */
  type CallGraph = Graph[String, DiEdge]

  /** Graphs form monoids. */
  implicit val CallGraphMonoid: Monoid[CallGraph] = new Monoid[CallGraph] {
    def append(a: CallGraph, b: => CallGraph): CallGraph = a ++ b
    def zero: CallGraph = Graph.empty
  }

  /** Make a local call graph containing a caller with its callees.
    *
    * @param caller The name of the caller.
    * @param callees The names of the callees.
    * @return The corresponding local call graph.
    */
  def local_call_graph(caller: String, callees: Set[String]): CallGraph =
    Graph.from(Seq(caller), callees map { DiEdge(caller, _) })

  /** Get a description of a graph in the dot format.
    *
    * @param graph The graph to be serialized.
    * @return A string containing a description of the graph in the dot format.
    */
  def to_dot(graph: CallGraph): String = { // BUG: Functions that call no functions and are not called never show up!
    import scalax.collection.io.dot._

    val dotRoot = DotRootGraph(directed = true, id = Some(Id("Call graph")))

    def edge_transformer(innerEdge: CallGraph#EdgeT): Option[(DotGraph, DotEdgeStmt)] =
      innerEdge.edge match {
        case DiEdge(source, target) =>
          Some(dotRoot -> DotEdgeStmt(
            NodeId(quote(source.toString.replace(".", "\n"))),
            NodeId(quote(target.toString.replace(".", "\n")))
          ))
      }

    graph.toDot(dotRoot, edge_transformer)
  }
}
