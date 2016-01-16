package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.{Field, Model, Service}

/**
  * For all models, except for those whose name ends in _form", common
  * fields should not have descriptions. This enables us to generate
  * consistent documenetation without worrying about whether a
  * particular description adds anything useful.
  */
case object CommonFieldsHaveNoDescriptions extends Linter with Helpers {

  val NamesWithNoDescriptions = Seq("id", "number")

  override def validate(service: Service): Seq[String] = {
    service.models.filter(!_.name.endsWith("_form")).flatMap(validateModel(service, _))
  }

  def validateModel(service: Service, model: Model): Seq[String] = {
    model.fields.flatMap(validateFieldDescription(service, model, _))
  }

  def validateFieldDescription(service: Service, model: Model, field: Field): Seq[String] = {
    field.description match {
      case None => {
        Nil
      }
      case Some(desc) => {
        NamesWithNoDescriptions.contains(field.name) match {
          case false => {
            Nil
          }
          case true => {
            Seq(error(model, field, "Must not have a description"))
          }
        }
      }
    }
  }

}
