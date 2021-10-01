package io.flow.oneapi

case class Module(name: String, serviceNames: Set[String]) {
  assert(
    name.toLowerCase() == name,
    "Module name must be in lower case"
  )
  assert(
    serviceNames.forall { n => n.toLowerCase() == n },
    "All service names must be in lower case"
  )
}

object Module {

  val General: Module = Module("general", Set("common", "feed", "healthcheck", "link", "organization", "search", "session", "token", "user"))
  val Webhook: Module = Module("webhook", Set("webhook"))

  val All = Seq(
    Module("localization", Set("catalog", "experience")),
    Module("pricing", Set("currency")),
    Module("landed cost", Set("harmonization")),
    Module("payment", Set("payment")),
    Module("logistics", Set("fulfillment", "inventory", "label", "ratecard", "return", "tracking")),
    Webhook,
    Module("customer service", Set.empty),
    Module("geolocation", Set("location")),
    Module("reference", Set("reference")),
    Module("partner", Set("partner")),
    General
  )

  def findByServiceName(name: String): Option[Module] = {
    All.find(_.serviceNames.contains(name.toLowerCase))
  }

  def findByModuleName(name: String): Option[Module] = {
    All.find(_.name == name.toLowerCase)
  }

  def moduleSortIndex(name: String): Int = {
    findByModuleName(name) match {
      case Some(module) => All.indexOf(module)
      case None => Int.MaxValue
    }
  }
}
