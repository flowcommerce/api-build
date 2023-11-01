package io.flow.lint.linters

import io.flow.lint.Linter
import io.apibuilder.spec.v0.models.{Model, Service}

/** For models w/ field named organization, ensure organization's position is:
  *   a. fourth (if first 3 fields are id, timestamp, type), to support journals b. second field (after id) c. first
  *      field (if no id)
  */
case object ModelsWithOrganizationField extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.models.flatMap(validateModel)
  }

  def validateModel(model: Model): Seq[String] = {
    val fieldNames = model.fields.filter(_.required).map(_.name).toList
    val index = fieldNames.indexOf("organization")

    if (index < 0) {
      Nil
    } else {
      val position = fieldNames.take(3) match {
        case "event_id" :: "timestamp" :: "organization" :: _ => 2
        case "event_id" :: "timestamp" :: "id" :: _ => 3
        case "id" :: "timestamp" :: "type" :: _ => 3
        case "id" :: _ => 1
        case _ => 0
      }

      if (position == index) {
        Nil
      } else {
        Seq(error(model, s"Field[organization] must be in position[$position] and not[$index]"))
      }
    }
  }

}
