package io.flow.build

case class Application(
  organization: String,
  application: String,
  version: String
) {
  val label = version match {
    case Application.Latest => "$organization/$application"
    case _ => "$organization/$application:$version"
  }
}

object Application {

  val Latest = "latest"

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
