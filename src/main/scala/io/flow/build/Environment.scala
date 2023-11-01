package io.flow.build

import scala.util.{Failure, Success, Try}

case class ApibuilderProfile(name: String, baseUrl: String, token: Option[String] = None)

/** Parses the API Builder configuration file
  */
object ApibuilderConfig {

  private[this] val DefaultPath = "~/.apibuilder/config"

  private[this] val DefaultApibuilderProfile = ApibuilderProfile(
    name = "default",
    baseUrl = "https://api.apibuilder.io",
    token = None
  )

  /** Loads API Builder configuration from the API Builder configuration file, returning either an error or the
    * configuration. You can set the APIBUILDER_PROFILE environment variable if you want to parse a specific profile.
    *
    * @param path
    *   The path to the configuration file we are reading
    */
  def load(
    path: String = DefaultPath
  ): Either[String, ApibuilderProfile] = {
    val profileName = Environment.optionalString("APIBUILDER_PROFILE").getOrElse(DefaultApibuilderProfile.name)
    val envToken = Environment.optionalString("APIBUILDER_TOKEN")
    val envBaseUrl = Environment.optionalString("APIBUILDER_API_BASE_URL")

    val profileOrErrors: Either[String, ApibuilderProfile] = loadAllProfiles(path) match {
      case Left(errors) => {
        Left(errors)
      }

      case Right(profiles) => {
        profiles.find(_.name == profileName) match {
          case None => {
            if (profileName == DefaultApibuilderProfile.name) {
              Right(DefaultApibuilderProfile)
            } else {
              Left(s"API Builder profile named[$profileName] not found")
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
            println("Using API Builder token from environment variable")
            profile.copy(token = Some(token))
          }
        }

        val p3 = envBaseUrl match {
          case None => p2
          case Some(url) => {
            println(s"Using API Builder baseUrl[$url] from environment variable")
            profile.copy(baseUrl = url)
          }
        }

        Right(p3)
      }
    }
  }

  private[this] val Profile = """\[profile (.+)\]""".r
  private[this] val Default = """\[default\]""".r

  private[this] def loadAllProfiles(path: String): Either[String, Seq[ApibuilderProfile]] = {
    val fullPath = path.replaceFirst("^~", System.getProperty("user.home"))
    val allProfiles = scala.collection.mutable.ListBuffer[ApibuilderProfile]()

    Try(
      if (new java.io.File(fullPath).exists) {
        var currentProfile: Option[ApibuilderProfile] = None

        scala.io.Source.fromFile(fullPath).getLines().map(_.trim).foreach {
          case Profile(name) => {
            currentProfile.map { p => allProfiles += p }
            currentProfile = Some(ApibuilderProfile(name = name, baseUrl = DefaultApibuilderProfile.baseUrl))
          }
          case Default() => {
            currentProfile.map { p => allProfiles += p }
            currentProfile = Some(DefaultApibuilderProfile)
          }
          case l => {
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

        currentProfile.map { p => allProfiles += p }
      }
    ) match {
      case Success(_) => {
        Right(allProfiles.toSeq)
      }
      case Failure(ex) => Left(ex.toString)
    }
  }

}

/** Helper to read configuration from environment
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
