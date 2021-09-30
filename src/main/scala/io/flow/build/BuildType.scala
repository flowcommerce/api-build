package io.flow.build

sealed trait BuildType {
  def oneApi: Boolean
  def proxy: Boolean
  def key: String
  def name: String
  def namespace: String

  // added for backwards compatibility as early versions of api-build relied on
  // toString being the key
  final override def toString: String = key
}

object BuildType {

  case object Api extends BuildType {
    override def oneApi: Boolean = true
    override def proxy: Boolean = true
    override def key = "api"
    override def name = "API"
    override def namespace = "io.flow"
  }
  case object ApiEvent extends BuildType {
    override def oneApi: Boolean = true
    override def proxy: Boolean = true
    override def key = "api-event"
    override def name = "API Event"
    override def namespace = "io.flow.event"
  }
  case object ApiInternal extends BuildType {
    override def oneApi: Boolean = true
    override def proxy: Boolean = true
    override def key = "api-internal"
    override def name = "API Internal"
    override def namespace = "io.flow.internal"
  }
  case object ApiInternalEvent extends BuildType {
    override def oneApi: Boolean = true
    override def proxy: Boolean = true
    override def key = "api-internal-event"
    override def name = "API Internal Event"
    override def namespace = "io.flow.internal.event"
  }
  case object ApiMisc extends BuildType {
    override def oneApi: Boolean = false
    override def proxy: Boolean = false
    override def key = "api-misc"
    override def name = "API Misc"
    override def namespace = "io.flow.misc"
  }
  case object ApiMiscEvent extends BuildType {
    override def oneApi: Boolean = false
    override def proxy: Boolean = false
    override def key = "api-misc-event"
    override def name = "API Misc Event"
    override def namespace = "io.flow.misc.event"
  }
  case object ApiPartner extends BuildType {
    override def oneApi: Boolean = true
    override def proxy: Boolean = true
    override def key = "api-partner"
    override def name = "API Partner"
    override def namespace = "io.flow.partner"
  }

  val all = Seq(Api, ApiEvent, ApiInternal, ApiInternalEvent, ApiMisc, ApiMiscEvent, ApiPartner)

  private[this] val byName = all.map(x => x.toString.toLowerCase -> x).toMap

  def fromString(value: String): Option[BuildType] = byName.get(value.toLowerCase)
    
}
