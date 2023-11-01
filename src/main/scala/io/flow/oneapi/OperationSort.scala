package io.flow.oneapi

import io.apibuilder.spec.v0.models.{Method, Operation}

object OperationSort {

  /** Sort the operations in order so that any statically declared references sort before equivalent dynamically
    * declared references. For example:
    *
    *   - /:organization/experiences/:key
    *   - /:organization/experiences/items
    *
    * We need the /items path to sort before /:key else it never resolves
    */
  def key(op: Operation): String = {
    (
      op.path.split("/").filter(_.nonEmpty).map { p =>
        if (p.startsWith(":")) {
          // Path component has a dynamic element in it - push to end
          s"z_$p"
        } else {
          s"a_$p"
        }
      } ++ Seq(methodSortOrder(op.method).toString)
    ).mkString(":")
  }

  /** Returns a numeric index by which we can sort methods. This allows us to present, for example, all operations with
    * a GET Method first.
    */
  private[this] def methodSortOrder(method: Method): Int = {
    method match {
      case Method.Get => 1
      case Method.Post => 2
      case Method.Put => 3
      case Method.Patch => 4
      case Method.Delete => 5
      case Method.Connect => 6
      case Method.Head => 7
      case Method.Options => 8
      case Method.Trace => 9
      case Method.UNDEFINED(_) => 10
    }
  }

}
