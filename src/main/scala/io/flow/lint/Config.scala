package io.flow.lint

/**
  * Helper to read configuration from environment
  */
object Config extends App {

  val apidocApiHost = optionalString("APIDOC_API_HOST").getOrElse("http://api.apidoc.me")
  val apidocApiToken = requiredString("APIDOC_API_TOKEN")

  println(s"apidocApiHost[$apidocApiHost]")
  println(s"apidocApiToken[$apidocApiToken]")

  val client = new com.bryzek.apidoc.api.v0.Client(
    apiUrl = apidocApiHost,
    auth = Some(
      com.bryzek.apidoc.api.v0.Authorization.Basic(apidocApiToken)
    )
  )

  import scala.concurrent.ExecutionContext.Implicits.global
  client.organizations.get().map { orgs =>
    orgs.foreach { org =>
      println(s"org: ${org.name}")
    }
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
