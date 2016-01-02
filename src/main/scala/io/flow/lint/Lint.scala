package io.flow.lint

import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.spec.v0.models.json._
import play.api.libs.json._


case class Lint(service: Service) {

  service.models.foreach { m =>
    println(s"m: ${m.name}")
  }

}

object Lint extends App {

  fromFile("/tmp/organization.json")

  def fromFile(path: String): Lint = {
    val contents = scala.io.Source.fromFile(path).getLines.mkString("\n")
    val service = Json.parse(contents).as[Service]
    Lint(service)
  }

}
