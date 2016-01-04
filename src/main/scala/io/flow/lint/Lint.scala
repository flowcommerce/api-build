package io.flow.lint

import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.spec.v0.models.json._
import play.api.libs.json._

case class Lint(
  linters: Seq[Linter] = Lint.All
) {
  
  def validate(service: Service): Seq[String] = {
    linters.map(_.validate(service)).flatten
  }

}

object Lint {

  val All = Seq(
    linters.Get,
    linters.Healthcheck,
    linters.ModelsWithOrganizationField,
    linters.SortParameterDefault,
    linters.StandardResponse,
    linters.VersionModels
  )

  def fromFile(path: String): Seq[String] = {
    val contents = scala.io.Source.fromFile(path).getLines.mkString("\n")
    val service = Json.parse(contents).as[Service]
    Lint(All).validate(service)
  }

  def main(args: Array[String]): Unit = {
    Downloader.withClient { dl =>

      import scala.concurrent.ExecutionContext.Implicits.global

      args.foreach { name =>
        val (organization, application, version) = name.split("/").map(_.trim).toList match {
          case org :: app :: Nil => (org, app, "latest")
          case org :: app :: version :: Nil => (org, app, version)
          case _ => {
            sys.error(s"Invalid name[$name] - expected organization/application (e.g. flow/user)")
          }
        }

        println("")
        println(s"Downloading $name")
        dl.service(organization, application, version) match {
          case Left(error) => println("  ** ERROR: " + error)
          case Right(service) => {
            println("  Success! Starting Linter:")
            Lint().validate(service) match {
              case Nil => println("  Valid!")
              case errors => {
                println("  1 or more errors:")
                errors.foreach { error =>
                  println(s"    - $error")
                }
              }
            }
          }
        }
      }

    }
  }

}
