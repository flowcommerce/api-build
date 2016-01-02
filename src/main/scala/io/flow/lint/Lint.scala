package io.flow.lint

import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.spec.v0.models.json._
import play.api.libs.json._

case class Lint(service: Service) {

  def validate(): Seq[String] = {
    Lint.All.map(_.validate(service)).flatten
  }

}

object Lint extends App {

  val All = Seq(
    linters.Get
  )

  def fromFile(path: String): Lint = {
    val contents = scala.io.Source.fromFile(path).getLines.mkString("\n")
    val service = Json.parse(contents).as[Service]
    Lint(service)
  }

  fromFile("/tmp/organization.json").validate() match {
    case Nil => println("valid")
    case errors => {
      println("1 or more errors:")
      errors.foreach { err =>
        println(s"  - $err")
      }
    }
  }

}
