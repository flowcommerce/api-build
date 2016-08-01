package io.flow.oneapi

case class Module(name: String, serviceNames: Seq[String])

object Module {

  val General = Module("general", Seq("common", "location", "organization", "search", "token", "user"))

  val All = Seq(
    Module("localization", Seq("catalog", "experience")),
    Module("pricing", Seq()),
    Module("landed cost", Seq("harmonization")),
    Module("payment", Seq("payment")),
    Module("logistics", Seq("delivery_window", "fulfillment", "inventory", "tracking")),
    Module("customer service", Nil),
    Module("reference", Seq("reference")),
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
