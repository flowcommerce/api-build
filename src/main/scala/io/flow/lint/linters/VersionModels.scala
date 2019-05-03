package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Field, Model, Service}

/**
  * Keep all _version models w/ same structure - 3 leading fields, then a
  * typed one
  * 
  *     "user_version": {
  *        "fields": [
  * 		{ "name": "id", "type": "string" },
  * 		{ "name": "timestamp", "type": "date-time-iso8601" },
  * 		{ "name": "type", "type": "io.flow.common.v0.enums.change_type" },
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

    model.fields.size == 4 match {
      case true => {
        Seq(
          findField(model, 0, "id"),
          findField(model, 1, "timestamp"),
          findField(model, 2, "type"),
          findField(model, 3, baseModelName)
        ).flatten match {
          case idField :: timestampField :: typeField :: modelField :: Nil => {
            validateType(model, idField, "string") ++
            validateType(model, timestampField, "date-time-iso8601") ++
            validateType(model, typeField, "io.flow.common.v0.enums.change_type") ++
            validateTypes(model, modelField, Seq(
              baseModelName,
              s"${baseModelName}_summary",
              s"expandable_$baseModelName"
            ))
          }
          case _ => {
            Seq(error(model, s"Must have exactly 4 fields: id, timestamp, type, $baseModelName"))
          }
        }
      }
      case false => {
        Seq(error(model, s"Must have exactly 4 fields: id, timestamp, type, $baseModelName"))
      }
    }
  }

  private[this] def findField(model: Model, index: Int, name: String): Option[Field] = {
    model.fields.lift(index).flatMap { f =>
      f.name == name match {
        case true => Some(f)
        case false => None
      }
    }
  }

  private[this] def validateType(model: Model, field: Field, datatype: String): Seq[String] = {
    validateTypes(model, field, Seq(datatype))
  }

  private[this] def validateTypes(model: Model, field: Field, datatypes: Seq[String]): Seq[String] = {
    assert(!datatypes.isEmpty)
    datatypes.find { datatype =>
      field.`type` == datatype || field.`type`.endsWith(s".$datatype") || field.`type`.endsWith(s".expandable_$datatype")
    } match {
      case None => {
        Seq(
          error(model, field, s"Must have type ${datatypes.mkString(" or ")} and not ${field.`type`}")
        )
      }
      case _ => {
        field.required match {
          case false => Seq(error(model, field, s"Must be required"))
          case true => Nil
        }
      }
    }
  }

}
