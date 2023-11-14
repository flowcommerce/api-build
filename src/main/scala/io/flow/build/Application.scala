package io.flow.build

case class Application(
  organization: String,
  application: String,
  version: String,
) {
  val isLatest: Boolean = version == Application.Latest

  val applicationVersionLabel: String = if (isLatest) {
    s"$application:latest"
  } else {
    s"$application:$version"
  }

  val label: String = s"$organization/$applicationVersionLabel"
}

object Application {

  val Latest = "latest"

  def latest(organization: String, application: String): Application = {
    Application(
      organization = organization,
      application = application,
      version = Latest,
    )
  }

  def parse(value: String): Option[Application] = {
    value.split("/").map(_.trim).toList match {
      case org :: app :: Nil => {
        app.split(":").map(_.trim).toList match {
          case Nil => {
            None
          }

          case name :: Nil => {
            Some(Application(org, name, Latest))
          }

          case name :: version :: Nil => {
            Some(Application(org, name, version))
          }

          case _ => {
            None
          }
        }
      }

      case _ => {
        None
      }
    }
  }

}
