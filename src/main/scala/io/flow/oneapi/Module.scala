package io.flow.oneapi

case class Module(name: String, serviceNames: Seq[String])

object Module {

  val General = Module("general", Seq("common", "feed", "healthcheck", "link", "organization", "search", "session", "token", "user"))
  val Webhook = Module("webhook", Seq("webhook"))

  val All = Seq(
    Module("localization", Seq("catalog", "experience")),
    Module("pricing", Seq("currency")),
    Module("landed cost", Seq("harmonization")),
    Module("payment", Seq("payment")),
    Module("logistics", Seq("fulfillment", "inventory", "label", "ratecard", "return", "tracking")),
    Webhook,
    Module("customer service", Nil),
    Module("geolocation", Seq("location")),
    Module("reference", Seq("reference")),
    Module("partner", Seq("partner")),
    General
  )

  def findByServiceName(name: String): Option[Module] = {
    All.find(_.serviceNames.contains(name.toLowerCase))
  }

  def findByModuleName(name: String): Option[Module] = {
    All.find(_.name == name.toLowerCase)
  }

  def moduleSortIndex(name: String): Int = {
    val module = findByModuleName(name).getOrElse {
      sys.error(s"Invalid module[$name]")
    }

    val i = All.indexOf(module)
    assert(i >= 0, s"Could not find module: $module")
    i
  }
}
