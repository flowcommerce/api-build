package io.flow.build

sealed trait BuildType {
  def oneapi: Boolean = true
  def proxy: Boolean = true
}

object BuildType {

  case object Api extends BuildType { override def toString = "api" }
  case object ApiEvent extends BuildType { override def toString = "api-event" }
  case object ApiInternal extends BuildType { override def toString = "api-internal" }
  case object ApiInternalEvent extends BuildType { override def toString = "api-internal-event" }
  case object ApiMisc extends BuildType {
    override def oneapi: Boolean = false
    override def proxy: Boolean = false
    override def toString = "api-misc"
  }
  case object ApiMiscEvent extends BuildType {
    override def oneapi: Boolean = false
    override def proxy: Boolean = false
    override def toString = "api-misc-event"
  }
  case object ApiPartner extends BuildType { override def toString = "api-partner" }

  val all = Seq(Api, ApiEvent, ApiInternal, ApiInternalEvent, ApiMisc, ApiMiscEvent, ApiPartner)

  private[this] val byName = all.map(x => x.toString.toLowerCase -> x).toMap

  def fromString(value: String): Option[BuildType] = byName.get(value.toLowerCase)
    
}
