package io.flow.build

sealed trait BuildType

object BuildType {

  case object Api extends BuildType { override def toString = "api" }
  case object Internal extends BuildType { override def toString = "internal" }

  val all = Seq(Api, Internal)

  private[this] val byName = all.map(x => x.toString.toLowerCase -> x).toMap

  def fromString(value: String): Option[BuildType] = byName.get(value.toLowerCase)
    
}
