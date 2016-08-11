package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Model, Service}

/**
  * For models w/ field named organization, ensure organization's position is:
  *   a. fourth (if first 3 fields are id, timestamp, type), to
  *      support journals
  *   b. second field (after id)
  *   c. first field (if no id)
  */
case object ModelsWithOrganizationField extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.models.flatMap(validateModel(_))
  }

  def validateModel(model: Model): Seq[String] = {
    val fieldNames = model.fields.map(_.name)
    val index = fieldNames.indexOf("organization")

    index < 0 match {
      case true => {
        Nil
      }
      case false => {
        val position = fieldNames.take(3) match {
          case "event_id" :: "timestamp" :: "organization" :: rest => 2
          case "id" :: "timestamp" :: "type" :: Nil => 3
          case "id" :: rest => 1
          case _ => 0
        }

        position == index match {
          case true => Nil
          case false => {
            Seq(error(model, s"Field[organization] must be in position[$position] and not[$index]"))
          }
        }
      }
    }
  }

}
