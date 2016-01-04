package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Model, Service}

/**
  * Keep all _version models w/ same structure - 3 leading fields, then a
  * typed one
  * 
  *     "user_version": {
  *        "fields": [
  * 		{ "name": "id", "type": "string" },
  * 		{ "name": "timestamp", "type": "date-time-iso8601" },
  * 		{ "name": "type", "type": "change_type" },
  * 		{ "name": "user", "type": "user" }
  * 	    ]
  * 	}
  */
case object VersionModels extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.models.filter(_.name.endsWith("_version")).flatMap(validateModel(_))
  }

  def validateModel(model: Model): Seq[String] = {
    assert(model.name.endsWith("_version"))
    val baseModelName = model.name.replace("_version", "")

    model.fields.map(_.name) match {
      case "id" :: "timestamp" :: "type" :: fourth :: Nil => {
        fourth == baseModelName match {
          case false => {
            Seq(error(model, s"Must have exactly 4 fields: id, timestamp, type, $baseModelName - and not id, timestamp, type, $fourth"))
          }
          case true => {
            validateType(model, "id", "string") ++
            validateType(model, "timestamp", "date-time-iso8601") ++
            validateType(model, "type", "io.flow.common.v0.enums.change_type") ++
            validateType(model, baseModelName, baseModelName)
          }
        }
      }
      case other => {
        Seq(error(model, s"Must have exactly 4 fields: id, timestamp, type, $baseModelName"))
      }
    }
  }

  def validateType(model: Model, fieldName: String, datatype: String): Seq[String] = {
    model.fields.find(_.name == fieldName) match {
      case None => Seq(error(model, s"Missing field[$fieldName]"))
      case Some(f) => {
        f.`type` == datatype match {
          case true => Nil
          case false => {
            Seq(error(model, s"Field[$fieldName] must have type ${datatype} and not ${f.`type`}"))
          }
        }
      }
    }
  }

}
