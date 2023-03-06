package io.flow.lint.linters

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits._
import io.apibuilder.spec.v0.models.{Field, Model, Service}
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
      case Invalid(e) => e.toList.distinct
      case Valid(_) => Nil
    }
  }

  private[this] def validate(event: EventInstance): ValidatedNec[String, Unit] = {
    (
      validateMatchingDeleteEvents(event),
      validateDeleteEventsHaveId(event.deleted),
      validateNoAdditionalFields(event)
    ).mapN { case (_, _, _) => () }
  }

  private[this] def validateMatchingDeleteEvents(event: EventInstance): ValidatedNec[String, Unit] = {
    event.upserted.map { m =>
      validateMatchingDeleteEvent(m, event.deleted)
    }.sequence.map(_ => ())
  }

  private[this] def validateMatchingDeleteEvent(upserted: UpsertedEventModel, candidates: Seq[DeletedEventModel]): ValidatedNec[String, Unit] = {
    candidates.find(_.prefix == upserted.prefix) match {
      case None => s"Missing delete event for '${upserted.model.name}'".invalidNec
      case Some(_) => ().validNec
    }
  }

  private[this] def validateDeleteEventsHaveId(models: Seq[DeletedEventModel]): ValidatedNec[String, Unit] = {
    models.map { m =>
      m.model.fields.find(_.name == "id") match {
        case None => s"Deleted event '${m.model.name}' is missing a field named 'id'".invalidNec
        case Some(f) => validateIdField(m.model, f)
      }
    }.sequence.map(_ => ())
  }

  private[this] def validateIdField(model: Model, field: Field): ValidatedNec[String, Unit] = {
    if (field.`type` == "string") {
      ().validNec
    } else {
      s"Model '${model.name}' Field '${field.name}' must have type 'string' and not '${field.`type`}'".invalidNec
    }
  }

  private[this] def validateNoAdditionalFields(event: EventInstance): ValidatedNec[String, Unit] = {
    (
      validateNoAdditionalFieldsUpserted(event.upserted),
      validateNoAdditionalFieldsDeleted(event.deleted)
    ).mapN { case (_, _) => () }
  }

  private[this] def validateNoAdditionalFieldsUpserted(models: Seq[UpsertedEventModel]): ValidatedNec[String, Unit] = {
    models.map { m =>
      validateNoAdditionalFields(m, Seq(m.prefix))
    }.sequence.map(_ => ())
  }

  private[this] def validateNoAdditionalFieldsDeleted(models: Seq[DeletedEventModel]): ValidatedNec[String, Unit] = {
    models.map { m =>
      validateNoAdditionalFields(m, Seq("id"))
    }.sequence.map(_ => ())
  }

  private[this] def validateNoAdditionalFields(m: EventModel, acceptableFinalFieldNames: Seq[String]): ValidatedNec[String, Unit] = {
    m.model.fields.zipWithIndex.map { case (field, i) =>
      i match {
        case 0 => validateFieldName(m, field, Seq("event_id"))
        case 1 => validateFieldName(m, field, Seq("timestamp"))
        case 2 => validateFieldName(m, field, acceptableFinalFieldNames ++ Seq("organization", "channel", "partner"))
        case 3 => validateFieldName(m, field, acceptableFinalFieldNames)
        case _ => error(m.model, "Cannot have more than 4 fields").invalidNec
      }
    }.sequence.map(_ => ())
  }

  private[this] def validateFieldName(model: EventModel, field: Field, allowed: Seq[String]): ValidatedNec[String, Unit] = {
    if (allowed.contains(field.name)) {
      ().validNec
    } else {
      invalidFieldError(model, field, allowed).invalidNec
    }
  }
  private[this] def invalidFieldError(model: EventModel, field: Field, allowed: Seq[String]): String = {
    error(model.model, field, s"Invalid name '${field.name}'. Must be one of: " + allowed.mkString(", "))
  }
}
