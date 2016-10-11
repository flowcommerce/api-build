package io.flow.build

sealed trait BuildType

object BuildType {

  case object Api extends BuildType { override def toString = "api" }
  case object ApiEvent extends BuildType { override def toString = "api-event" }
  case object ApiInternal extends BuildType { override def toString = "api-internal" }
  case object ApiInternalEvent extends BuildType { override def toString = "api-internal-event" }
  case object ApiPartner extends BuildType { override def toString = "api-partner" }

  val all = Seq(Api, ApiEvent, ApiInternal, ApiInternalEvent, ApiPartner)

  private[this] val byName = all.map(x => x.toString.toLowerCase -> x).toMap

  def fromString(value: String): Option[BuildType] = byName.get(value.toLowerCase)
    
}
