package io.flow.build

import cats.implicits._
import io.circe.generic.auto._
import io.circe.yaml

import scala.io.Source
import scala.util.Using

case class ServerConfig(name: String, host: String)

object ServerConfig {
  def parseFile(path: java.nio.file.Path): Either[String, Seq[ServerConfig]] =
    Using.resource(Source.fromFile(path.toUri)) { source =>
      parseYaml(source.mkString)
    }

  def parseYaml(contents: String): Either[String, Seq[ServerConfig]] = {
    val parseResult = for {
      json <- yaml.parser.parse(contents)
      serverConfigs <- json.hcursor.downField("servers").as[List[ServerConfig]]
    } yield serverConfigs

    parseResult.leftMap(_.show)
  }
}
