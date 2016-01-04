package io.flow.lint

/**
  * Helper to read configuration from environment
  */
object Config extends App {

  lazy val apidocApiHost = optionalString("APIDOC_API_HOST").getOrElse("http://api.apidoc.me")
  lazy val apidocApiToken = requiredString("APIDOC_API_TOKEN")

  lazy val apidocApiClient = {
    new com.bryzek.apidoc.api.v0.Client(
      apiUrl = apidocApiHost,
      auth = Some(
        com.bryzek.apidoc.api.v0.Authorization.Basic(apidocApiToken)
      )
    )
  }

  def requiredString(name: String): String = {
    optionalString(name).getOrElse {
      sys.error(s"Environment variable[$name] must be set")
    }
  }

  def optionalString(name: String): Option[String] = {
    sys.env.get(name).map(_.trim).map { value =>
      value match {
        case "" => {
          sys.error(s"Value for environment variable[$name] cannot be blank")
        }
        case _ => value
      }
    }
  }

}
