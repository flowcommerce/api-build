package io.flow.lint.linters

import io.apibuilder.spec.v0.models.{Model, Service}
import io.flow.lint.Linter
import io.flow.stream.EventUnionTypeMatcher

/**
  * Match naming convention required to get events into s3
  */
case object UpsertedDeletedEventModels extends Linter with Helpers {
  
  override def validate(service: Service): Seq[String] = {
    service.models.flatMap { m =>
      expectedFieldName(m.name).toSeq.flatMap { name =>
        validateModel(m, name)
      }
    }
  }

  def expectedFieldName(name: String): Option[String] = {
    val i = name.indexOf("_upserted")
    if (i > 0) {
      Some(name.take(i))
    } else {
      val j = name.indexOf("_deleted")
      if (j > 0) {
        Some(name.take(j))
      } else {
        None
      }
    }
  }

  def validateModel(model: Model, expected: String): Seq[String] = {
    if (model.fields.exists { f =>
      EventUnionTypeMatcher.matchFieldToPayloadType(f, expected)
    }) {
      Nil
    } else {
      Seq(
        error(model, s"Event must contain a field whose name and type contain " + expected.split("_").mkString(" or "))
      )
    }
  }
  
}
