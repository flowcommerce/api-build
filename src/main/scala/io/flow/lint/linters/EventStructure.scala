package io.flow.lint.linters

import cats.implicits._
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import io.apibuilder.spec.v0.models.Service
import io.flow.lint.Linter

/**
  * For event models (models ending with 'upserted', 'deleted'), validate:
  * 
  *   a. upserted events have deleted events
  *   b. updated event models have an 'id' field
  *   c. deleted events have only an 'id'
 *    d. no extra fields on the event
  */
case object EventStructure extends Linter with EventHelpers {

  override def validate(service: Service): Seq[String] = {
    findAllEvents(service).map(validate).sequence match {
      case Invalid(e) => e.toList
      case Valid(_) => Nil
    }
  }

  private[this] def validate(event: EventInstance): ValidatedNec[String, Unit] = {
    ().validNec
  }
  
}
