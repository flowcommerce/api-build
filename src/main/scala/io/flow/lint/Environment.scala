package io.flow.lint

/**
  * Helper to read configuration from environment
  */
object Environment {

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
