package io.flow.oneapi

object Util {
  def namespaceTypeName(ns: String, t: String) =
    (ns.split('.') ++ t.split('_')).mkString("_")
}
