package io.flow.build

import scala.util.{Failure, Success, Try}

case class ApidocProfile(name: String, baseUrl: String, token: Option[String] = None)

/**
  * Parses the apidoc configuration file
  */
object ApidocConfig {

  private[this] val DefaultPath = "~/.apidoc/config"

  private[this] val DefaultApidocProfile = ApidocProfile(
    name = "default",
    baseUrl = "http://api.apidoc.me",
    token = None
  )

  /**
    * Loads apidoc configuration from the apidoc configuration file,
    * returning either an error or the configuration. You can set the
    * APIDOC_PROFILE environment variable if you want to parse a
    * specific profile.
    * 
    * @param path The path to the configuration file we are reading
    */
  def load(
    path: String = DefaultPath
  ): Either[String, ApidocProfile] = {
    val profileName = Environment.optionalString("APIDOC_PROFILE").getOrElse(DefaultApidocProfile.name)
    val envToken = Environment.optionalString("APIDOC_TOKEN")
    val envBaseUrl = Environment.optionalString("APIDOC_API_BASE_URL")

    val profileOrErrors: Either[String, ApidocProfile] = loadAllProfiles(path) match {
      case Left(errors) => {
        Left(errors)
      }

      case Right(profiles) => {
        profiles.find(_.name == profileName) match {
          case None => {
            profileName == DefaultApidocProfile.name match {
              case true => Right(DefaultApidocProfile)
              case false => Left(s"apidoc profile named[$profileName] not found")
            }
          }
          case Some(p) => {
            Right(p)
          }
        }
      }
    }

    profileOrErrors match {
      case Left(errors) => Left(errors)
      case Right(profile) => {
        val p2 = envToken match {
          case None => profile
          case Some(token) => {
            println("Using apidoc token from environment variable")
            profile.copy(token = Some(token))
          }
        }

        val p3 = envBaseUrl match {
          case None => p2
          case Some(url) => {
            println("Using apidoc baseUrl[$url] from environment variable")
            profile.copy(baseUrl = url)
          }
        }

        Right(p3)
      }
    }
  }

  private[this] val Profile = """\[profile (.+)\]""".r
  private[this] val Default = """\[default\]""".r

  private[this] def loadAllProfiles(path: String): Either[String, Seq[ApidocProfile]] = {
    val fullPath = path.replaceFirst("^~", System.getProperty("user.home"))
    var allProfiles = scala.collection.mutable.ListBuffer[ApidocProfile]()

    Try(
      if (new java.io.File(fullPath).exists) {
        var currentProfile: Option[ApidocProfile] = None

        scala.io.Source.fromFile(fullPath).getLines.map(_.trim).foreach { l =>
          l match {
            case Profile(name) => {
              currentProfile.map { p => allProfiles += p }
              currentProfile = Some(ApidocProfile(name = name, baseUrl = DefaultApidocProfile.baseUrl))
            }
            case Default() => {
              currentProfile.map { p => allProfiles += p }
              currentProfile = Some(DefaultApidocProfile)
            }
            case _ => {
              l.split("=").map(_.trim).toList match {
                case "token" :: value :: Nil => {
                  currentProfile = currentProfile.map(_.copy(token = Some(value)))
                }
                case "api_uri" :: value :: Nil => {
                  currentProfile = currentProfile.map(_.copy(baseUrl = value))
                }
                case _ => {
                  // ignore
                }
              }
            }
          }
        }

        currentProfile.map { p => allProfiles += p }
      }
    ) match {
      case Success(_) => {
        Right(allProfiles)
      }
      case Failure(ex) => Left(ex.toString)
    }
  }

}

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
