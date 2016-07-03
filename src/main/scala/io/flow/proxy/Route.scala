package io.flow.proxy

case class Route(method: String, path: String) extends Ordered[Route] {

  private val sortKey = s"${path.toLowerCase}:${method.toLowerCase}"

  def yaml = {
    s"$method $path"
  }

  override def compare(that: Route) = {
    sortKey.compare(that.sortKey)
  }
}
